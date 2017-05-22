package uk.ac.cdrc.data.utility.text

/**
  * Testing Similarity
  */

import org.scalatest._
import org.scalatest.prop.Checkers
import spire.std.{LevenshteinDistance => LD}

class SimilaritySpec extends FlatSpec with Matchers with Checkers {
  "Empties" should "work fine" in new LevenshteinStringDistance {
    distance("", "") should be (0d)
    distance("a", "") should be (1d)
    distance("", "b") should be (1d)
  }

  "Our Levenshtein distance" should "compare with spire.std.LevenshteinDistance" in new LevenshteinStringDistance {
    check((a: String, b: String) => distance(a, b) == LD.distance(a, b))
  }

  "Substrings" should "have a distance from the missing parts" in new LevenshteinStringDistance {
    check((a: String, b: String) => distance(a + b, b) == a.length)
    check((a: String, b: String) => distance(b + a, b) == a.length)
    check((a: String, b: String, c: String) => distance(a + b + c, b) == a.length + c.length)
  }

  "NumberSpanDistance" should "work" in new NumberSpanAnalyzer with NumberSpanDistance {
    implicit def toProcessed(s: String): IndexedSeq[String] = process(s)
    distance("aaa bbb ccc 1-5 ccc", "bbcc dd 1-3,4,5 ff") should be (0d)
    distance("aaa bbb ccc 1 ccc", "bbcc dd 1,5 ff") should be (1d)
    distance("aaa bbb ccc 1,5-6 ccc", "bbcc dd ff") should be (0d)
    distance("aaa bbb ccc ccc", "bbcc dd ff") should be (0d)

  }

  "NumbersOverlapDistance" should "work" in new NumberSpanAnalyzer with NumberOverlapDistance {
    implicit def toProcessed(s: String): IndexedSeq[String] = process(s)
    distance("aaa bbb ccc 1-5 ccc", "bbcc dd 1-3,4,5 ff") should be < 0.2d
    distance("aaa bbb ccc 1 ccc", "bbcc dd 1,5 ff") should be (0.5d)
    distance("aaa bbb ccc 1,5-6 ccc", "bbcc dd ff") should be (1d)
    distance("aaa bbb ccc ccc", "bbcc dd ff") should be (0d)
  }

  "WordBagDistance" should "work" in new WordBagAnalyzer with WordBagDistance {
    distance("a b c", "a b c") should be (0d)
    distance("a b c d", "a b c") should be (0d)
    distance("a b c d", "a b c c d") should be (1d)
  }

  "WordSetDistance" should "work" in new WordBagAnalyzer with WordSetDistance {
    distance("a b c", "a b c") should be (0d)
    distance("a b c d", "a b c") should be (0d)
    distance("a b c d", "a b c c d") should be (0d)
    distance("a b c d e", "a b c c d") should be (0d)
    distance("a b c d", "a b c c d e") should be (0.2d)
    distance("a b c d", "a b d e") should be (0.25d)
  }

  "LetterPrefixDistance" should "work" in new LetterPrefixDistance {
    distance("a b c", "a b c") should be (0d)
    distance("a b c d", "a b c") should be > 0.0d
    distance("a b c d", "a b c c d") should be < 0.34d
    distance("a b c d e", "a b c c d") should be < 0.4d
    distance("a b c d", "a b c c d e") should be < 0.46d
    distance("a b c d", "a b d e") should be < 0.5d
  }


  "WordPrefixDistance" should "work" in new WordSeqAnalyzer with WordPrefixDistance {
    implicit def toProcessed(s: String): IndexedSeq[String] = process(s)
    distance("a b c", "a b c") should be (0d)
    distance("a b c d", "a b c") should be (0.25d)
    distance("a b c d", "a b c c d") should be (0.4d)
    distance("a b c d e", "a b c c d") should be <= 0.4d
    distance("a b c d", "a b c c d e") should be (0.5d)
    distance("a b c d", "a b d e") should be (0.5d)
  }
}
