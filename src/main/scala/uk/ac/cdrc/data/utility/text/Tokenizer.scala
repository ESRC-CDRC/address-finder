package uk.ac.cdrc.data.utility.text

import scala.util.matching.Regex

/**
  * Tokenizers are used for creating unit tokens for matching
  */
trait Tokenizer {
  def tokenize(s: String): IndexedSeq[String]
}

trait NumPatterns {
  val numSpanPattern: Regex = "([1-9]\\d*)\\s*-\\s*(\\d+)".r
  val numPattern: Regex = "([1-9]\\d*)|0\\b".r
  val orderedNumPattern: Regex = "\\b(1st)|(2nd)|(3rd)\\b".r
  val alphabetPattern: Regex = "[1-9]\\d*([a-z])\\b".r
  val alphabetFlatPattern: Regex = "flat ([a-z])\\b".r
}

/**
  * This object provide a tokenizer that can cut between word and numbers
  * For example, "4a" will be come Seq("4", "a")
  * Other wise simple white space tokenizer is used.
  */
object DigitWordTokenizer extends Tokenizer {
  val separator: Regex = "\\s+|-+".r
  val digitsPattern: Regex = "\\d+".r
  override def tokenize(s: String): IndexedSeq[String] = {
    val digits = for (d <- (digitsPattern findAllIn s).matchData) yield d.toString
    val words = for (w <- separator split (digitsPattern replaceAllIn(s, " "))) yield w.toString
    (digits ++ words).toIndexedSeq filter {!_.isEmpty}
  }
}
