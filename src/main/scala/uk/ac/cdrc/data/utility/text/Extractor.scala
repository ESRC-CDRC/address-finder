package uk.ac.cdrc.data.utility.text

/**
  * Created  on 7/29/16.
  */
trait Extractor {
  def extract(s: String): IndexedSeq[String]
}

trait NumberSpanExtractor extends Extractor{
  val spanPattern = "(\\d+)\\s*-\\s*(\\d+)".r
  val numPattern = "\\d+".r

  override def extract(s: String): IndexedSeq[String] = {
    val numSpan = for {
      m <- (spanPattern findAllIn s).matchData
      i <- m.group(1).toLong to m.group(2).toLong
    } yield i.toString
    val nums = for {
      m <- numPattern findAllIn (spanPattern replaceAllIn(s, " "))
    } yield m.toString
    (numSpan ++ nums).toIndexedSeq
  }

}
