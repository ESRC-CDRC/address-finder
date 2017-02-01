package uk.ac.cdrc.data.utility.text

/**
  * Created  on 7/25/16.
  */

import org.scalatest._
import org.scalatest.prop.Checkers
import spire.std.{LevenshteinDistance => LD}
import uk.ac.cdrc.data.utility.text.entity.WordBag

class SimilaritySpec extends FlatSpec with Matchers with Checkers {
  "Empties" should "work fine" in new LevenshteinDistance {
    distance("", "") should be (0)
    distance("a", "") should be (1)
    distance("", "b") should be (1)
  }

  "Our Levenshtein distance" should "compare with spire.std.LevenshteinDistance" in new LevenshteinDistance {
    check((a: String, b: String) => distance(a, b) == LD.distance(a, b))
  }
  "Substrings" should "have a distance from the missing parts" in new LevenshteinDistance {
    check((a: String, b: String) => distance(a + b, b) == a.length)
    check((a: String, b: String) => distance(b + a, b) == a.length)
    check((a: String, b: String, c: String) => distance(a + b + c, b) == a.length + c.length)
  }

  "extractNumberSpan" should "work" in new NumberSpanAnalyzer{
    process("aaa bbb ccc 1-5 ccc") should be (Array("1", "2", "3", "4", "5"))
    process("aaa bbb ccc 1-5,6,7 ccc") should be (Array("1", "2", "3", "4", "5", "6", "7"))
    process("aaa bbb ccc 6,7 ccc") should be (Array("6", "7"))
    process("aaa bbb ccc 7 ccc") should be (Array("7"))
    process("aaa bbb ccc ccc") should be (Array())
  }

  "NumberSpanDistance" should "work" in new NumberSpanAnalyzer with NumberSpanDistance {
    implicit def toProcessed(s: String): IndexedSeq[String] = process(s)
    distance("aaa bbb ccc 1-5 ccc", "bbcc dd 1-3,4,5 ff") should be (0)
    distance("aaa bbb ccc 1 ccc", "bbcc dd 1,5 ff") should be (1)
    distance("aaa bbb ccc 1,5-6 ccc", "bbcc dd ff") should be (-1)
    distance("aaa bbb ccc ccc", "bbcc dd ff") should be (-1)

  }

  "NumbersOverlapDistance" should "work" in new NumberSpanAnalyzer with NumberOverlapDistance {
    implicit def toProcessed(s: String): IndexedSeq[String] = process(s)
    distance("aaa bbb ccc 1-5 ccc", "bbcc dd 1-3,4,5 ff") should be (0)
    distance("aaa bbb ccc 1 ccc", "bbcc dd 1,5 ff") should be (0.5f)
    distance("aaa bbb ccc 1,5-6 ccc", "bbcc dd ff") should be (1)
    distance("aaa bbb ccc ccc", "bbcc dd ff") should be (0)
  }

  "WordBagDistance" should "work" in new WordBagAnalyzer with WordBagDistance {
    distance("a b c", "a b c") should be (0)
    distance("a b c d", "a b c") should be (0)
    distance("a b c d", "a b c c d") should be (1)
  }

  "WordSetDistance" should "work" in new WordBagAnalyzer with WordSetDistance {
    distance("a b c", "a b c") should be (0)
    distance("a b c d", "a b c") should be (0)
    distance("a b c d", "a b c c d") should be (0)
    distance("a b c d e", "a b c c d") should be (0)
    distance("a b c d", "a b c c d e") should be (1)
    distance("a b c d", "a b d e") should be (1)
  }

  "LetterPrefixDistance" should "work" in new LevenshteinDistance {
    distance("a b c", "a b c") should be (0)
    distance("a b c d", "a b c") should be (0.0f)
    distance("a b c d", "a b c c d") should be < 0.2f
    distance("a b c d e", "a b c c d") should be < 0.4f
    distance("a b c d", "a b c c d e") should be < 0.2f
    distance("a b c d", "a b d e") should be < 0.5f
  }


  "WordPrefixDistance" should "work" in new WordSeqAnalyzer with WordPrefixDistance {
    implicit def toProcessed(s: String): IndexedSeq[String] = process(s)
    distance("a b c", "a b c") should be (0)
    distance("a b c d", "a b c") should be (0.0f)
    distance("a b c d", "a b c c d") should be (0.25f)
    distance("a b c d e", "a b c c d") should be < 0.4f
    distance("a b c d", "a b c c d e") should be (0.25f)
    distance("a b c d", "a b d e") should be (0.5f)
  }
}
