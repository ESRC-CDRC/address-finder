package uk.ac.cdrc.data.utility.text

import scala.util.matching.Regex

/**
  * Created  on 7/29/16.
  */
trait Tokenizer {
  def tokenize(s: String): IndexedSeq[String]
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
