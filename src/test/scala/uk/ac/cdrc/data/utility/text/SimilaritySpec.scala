package uk.ac.cdrc.data.utility.text

/**
  * Created  on 7/25/16.
  */

import org.scalatest._
import org.scalatest.prop.Checkers
import spire.std.{LevenshteinDistance => LD}

class SimilaritySpec extends FlatSpec with Matchers with Checkers {
  "Empties" should "work fine" in {
    LevenshteinDistance.distance("", "") should be (0)
    LevenshteinDistance.distance("a", "") should be (1)
    LevenshteinDistance.distance("", "b") should be (1)
  }

  "Our Levenshtein distance" should "compare with spire.std.LevenshteinDistance" in {
    check((a: String, b: String) => LevenshteinDistance.distance(a, b) == LD.distance(a, b))
  }
  "Substrings" should "have a distance from the missing parts" in {
    check((a: String, b: String) => LevenshteinDistance.distance(a + b, b) == a.length)
    check((a: String, b: String) => LevenshteinDistance.distance(b + a, b) == a.length)
    check((a: String, b: String, c: String) => LevenshteinDistance.distance(a + b + c, b) == a.length + c.length)
  }

  "extractNumberSpan" should "work" in {
    NumberSpanDistance.extractNumberSpan("aaa bbb ccc 1-5 ccc") should be (Set(1, 2, 3, 4, 5))
    NumberSpanDistance.extractNumberSpan("aaa bbb ccc 1-5,6,7 ccc") should be (Set(1, 2, 3, 4, 5, 6, 7))
    NumberSpanDistance.extractNumberSpan("aaa bbb ccc 6,7 ccc") should be (Set(6, 7))
    NumberSpanDistance.extractNumberSpan("aaa bbb ccc 7 ccc") should be (Set(7))
    NumberSpanDistance.extractNumberSpan("aaa bbb ccc ccc") should be (Set())
  }

  "NumberSpanDistance" should "work" in {
    NumberSpanDistance.distance("aaa bbb ccc 1-5 ccc", "bbcc dd 1-3,4,5 ff") should be (0)
    NumberSpanDistance.distance("aaa bbb ccc 1 ccc", "bbcc dd 1,5 ff") should be (1)
    NumberSpanDistance.distance("aaa bbb ccc 1,5-6 ccc", "bbcc dd ff") should be (-1)
    NumberSpanDistance.distance("aaa bbb ccc ccc", "bbcc dd ff") should be (-1)

  }

  "NumbersOverlapDistance" should "work" in {
    NumbersOverlapDistance.distance("aaa bbb ccc 1-5 ccc", "bbcc dd 1-3,4,5 ff") should be (0)
    NumbersOverlapDistance.distance("aaa bbb ccc 1 ccc", "bbcc dd 1,5 ff") should be (0.5f)
    NumbersOverlapDistance.distance("aaa bbb ccc 1,5-6 ccc", "bbcc dd ff") should be (1)
    NumbersOverlapDistance.distance("aaa bbb ccc ccc", "bbcc dd ff") should be (0)
  }

  "WordBagDistance" should "work" in {
    WordBagDistance.distance("a b c", "a b c") should be (0)
    WordBagDistance.distance("a b c d", "a b c") should be (0)
    WordBagDistance.distance("a b c d", "a b c c d") should be (1)
  }

  "WordSetDistance" should "work" in {
    WordSetDistance.distance("a b c", "a b c") should be (0)
    WordSetDistance.distance("a b c d", "a b c") should be (0)
    WordSetDistance.distance("a b c d", "a b c c d") should be (0)
    WordSetDistance.distance("a b c d e", "a b c c d") should be (0)
    WordSetDistance.distance("a b c d", "a b c c d e") should be (1)
    WordSetDistance.distance("a b c d", "a b d e") should be (1)
  }

  "CommonPrefixDistance" should "work" in {
    CommonPrefixDistance.distance("a b c", "a b c") should be (0)
    CommonPrefixDistance.distance("a b c d", "a b c") should be (0.0f)
    CommonPrefixDistance.distance("a b c d", "a b c c d") should be < 0.2f
    CommonPrefixDistance.distance("a b c d e", "a b c c d") should be < 0.4f
    CommonPrefixDistance.distance("a b c d", "a b c c d e") should be < 0.2f
    CommonPrefixDistance.distance("a b c d", "a b d e") should be < 0.5f
  }


  "WordCommonPrefixDistance" should "work" in {
    WordCommonPrefixDistance.distance("a b c", "a b c") should be (0)
    WordCommonPrefixDistance.distance("a b c d", "a b c") should be (0.0f)
    WordCommonPrefixDistance.distance("a b c d", "a b c c d") should be (0.25f)
    WordCommonPrefixDistance.distance("a b c d e", "a b c c d") should be < 0.4f
    WordCommonPrefixDistance.distance("a b c d", "a b c c d e") should be (0.25f)
    WordCommonPrefixDistance.distance("a b c d", "a b d e") should be (0.5f)
  }
}
