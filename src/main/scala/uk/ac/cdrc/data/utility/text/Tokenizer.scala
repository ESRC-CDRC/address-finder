package uk.ac.cdrc.data.utility.text

/**
  * Created  on 7/29/16.
  */
trait Tokenizer {
  def tokenize(s: String): IndexedSeq[String]
}

object SimpleTokenizer extends Tokenizer {
  val separator = "\\s+|-+".r
  val digitsPattern = "\\d+".r
  override def tokenize(s: String): IndexedSeq[String] = {
    val digits = for (d <- (digitsPattern findAllIn s).matchData) yield d.toString
    val words = for (w <- separator split (digitsPattern replaceAllIn(s, " "))) yield w.toString
    (digits ++ words).toIndexedSeq filter {!_.isEmpty}
  }
}
