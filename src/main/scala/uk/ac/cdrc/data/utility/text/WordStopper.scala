package uk.ac.cdrc.data.utility.text

import scala.annotation.tailrec

/**
  * A stop word processing, probably not useful
  */
trait WordStopper {
  private val wordToStop = Seq("flat")

  @tailrec
  private def _filter(s: String, stopList: Seq[String]): String = {
    stopList match {
      case h::ts =>
        val r = s.replaceAll(h, "")
        _filter(r, ts)
      case _ => s
    }
  }

  def filter(s: String): String = _filter(s, wordToStop)
}
