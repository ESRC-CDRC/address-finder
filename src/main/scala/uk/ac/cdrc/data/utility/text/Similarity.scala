package uk.ac.cdrc.data.utility.text

import breeze.linalg.{Counter, min}
import uk.ac.cdrc.data.utility.text.entity.WordBag
import WordBag._

/**
  * Created  on 7/25/16.
  */
trait Similarity {
  def distance(a: String, b: String): Float
}

object LevenshteinDistance extends Similarity {
  def distance[T <: IndexedSeq[_]](a: T, b: T): Float = {
    if (a.isEmpty) b.length
    else if (b.isEmpty) a.length
    else {
      val dist = Array.tabulate(a.length + 1, b.length + 1)((i, j) => if (i == 0 || j == 0) i + j else 0f)
      for (i <- 1 to a.length; j <- 1 to b.length) {
        dist(i)(j) = if (a(i-1) == b(j-1)) dist(i - 1)(j - 1) else min(dist(i - 1)(j), dist(i)(j - 1), dist(i - 1)(j - 1)) + 1
      }
      dist(a.length)(b.length)
    }
  }
  def distance(a: String, b: String): Float = distance(a.toVector, b.toVector)
}

object NumberSpanDistance extends Similarity {
  val spanPattern = "(\\d+)\\s*-\\s*(\\d+)".r
  val numPattern = "\\d+".r

  def extractNumberSpan(s: String): IndexedSeq[Int] = {
    val numSpan = for {
      m <- (spanPattern findAllIn s).matchData
      i <- m.group(1).toInt to m.group(2).toInt
    } yield i
    val nums = for {
      m <- numPattern findAllIn (spanPattern replaceAllIn(s, " "))
    } yield m.toInt
    (numSpan ++ nums).toIndexedSeq
  }

  override def distance(a: String, b: String): Float = LevenshteinDistance.distance(extractNumberSpan(a), extractNumberSpan(b))
}

object WordBagDistance extends Similarity {
  override def distance(a: String, b: String): Float = distance(WordBag(a), WordBag(b))
  def distance(a: Counter[String, Int], b: Counter[String, Int]): Float = (b - a).pnorm
}
