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


object NumberSpanDistance extends Similarity with NumberSpanExtractor{
  override def distance(a: String, b: String): Float = {
    val numSetB = extract(b)
    val numSetA = extract(a)
    distance(numSetA, numSetB)
  }

  def distance(a: IndexedSeq[String], b: IndexedSeq[String]): Float = {
    if (b.isEmpty)
      -1.0f // If the query do not contain any numbers then use -1f to mark it
    else
      (b.toSet -- a.toSet).size
  }
}

object NumbersOverlapDistance extends Similarity with NumberSpanExtractor {

  override def distance(a: String, b: String): Float = {
    val numSetB = extract(b)
    val numSetA = extract(a)
    distance(numSetA, numSetB)
  }
  def distance(a: IndexedSeq[String], b: IndexedSeq[String]): Float = {
    val aSet = a.toSet
    val bSet = b.toSet
    val union: Float = (bSet | aSet).size
    if (union == 0)
      0
    else
      1.0f - (bSet & aSet).size / (union: Float)
  }
}


object WordBagDistance extends Similarity {
  override def distance(a: String, b: String): Float = distance(WordBag(a), WordBag(b))
  def distance(a: Counter[String, Int], b: Counter[String, Int]): Float = (b - a).pnorm
}

object WordSetDistance extends Similarity {
  override def distance(a: String, b: String): Float = distance(WordBag(a), WordBag(b))
  def distance(a: Counter[String, Int], b: Counter[String, Int]): Float = (b.keySet -- a.keySet).size
}


object CommonPrefixDistance extends Similarity {
  override def distance(a: String, b: String): Float = distance(a.toSeq, b.toSeq)

  def distance[T](a: Seq[T], b: Seq[T]): Float = {
    val commonPrefixLength = (a zip b).takeWhile(v => v._1 == v._2).length
    1 - commonPrefixLength / (min(a.length, b.length): Float)
  }
}

object WordCommonPrefixDistance extends Similarity {
  val tokenizer = SimpleTokenizer
  override def distance(a: String, b: String): Float = CommonPrefixDistance.distance(tokenizer tokenize  a.trim, tokenizer tokenize  b.trim)
}