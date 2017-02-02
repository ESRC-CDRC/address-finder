package uk.ac.cdrc.data.utility.text

import breeze.linalg.min
import uk.ac.cdrc.data.utility.text.entity.WordBag
import uk.ac.cdrc.data.utility.text.entity.WordBag._

/**
  * Created  on 7/25/16.
  */
trait Similarity[T] {
  def distance(a: T, b: T): Double
}

trait LevenshteinDistance extends Similarity[String] {
  def distance[T <: IndexedSeq[_]](a: T, b: T): Double = {
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
  def distance(a: String, b: String): Double = distance(a.toVector, b.toVector)
}


trait NumberSpanDistance extends Similarity[IndexedSeq[String]] {

  def distance(a: IndexedSeq[String], b: IndexedSeq[String]): Double = {
    if (b.isEmpty)
      -1.0f // If the query do not contain any numbers then use -1f to mark it
    else
      (b.toSet -- a.toSet).size
  }
}

trait NumberOverlapDistance extends Similarity[IndexedSeq[String]]{

  def distance(a: IndexedSeq[String], b: IndexedSeq[String]): Double = {
    val aSet = a.toSet
    val bSet = b.toSet
    val union: Double = (bSet | aSet).size
    if (union == 0)
      0.0f
    else
      1.0f - (bSet & aSet).size / union
  }
}

trait StrictNumberOverlapDistance extends NumberOverlapDistance {
  override def distance(a: IndexedSeq[String], b: IndexedSeq[String]): Double = if (super.distance(a, b) < 1) 0 else 100
}

trait WordBagDistance extends Similarity[WordBag] {
  def distance(a: WordBag, b: WordBag): Double = (wordBag2Counter(b) - wordBag2Counter(a)).pnorm
}

trait WordSetDistance extends Similarity[WordBag] {
  def distance(a: WordBag, b: WordBag): Double = (b.keySet -- a.keySet).size
}

trait PriorityWordDistance extends Similarity[IndexedSeq[String]] {
  def posWeight[T](seq: Seq[T], w: T): Double = {
    val p = seq.indexOf(w)
    if (p >= 0) (seq.length - p) / seq.length
    else 0.0f
  }
  override def distance(a: IndexedSeq[String], b: IndexedSeq[String]): Double = {
    (for {
      w <- a.toSet -- b.toSet
    } yield posWeight(a, w) + posWeight(b, w)).sum
  }
}

trait SymmetricWordSetDistance extends Similarity[WordBag] {
  def distance(a: WordBag, b: WordBag): Double = (b.keySet -- a.keySet).size + (a.keySet -- b.keySet).size
}

trait SymmetricWordSetDistanceWithIDF extends Similarity[WordBag] {
  self: WordBagAnalyzedPoolWithIDF =>
  def distance(a: WordBag, b: WordBag): Double = (for {
    word <- (b.keySet -- a.keySet) ++ (a.keySet -- b.keySet)
  } yield idf(word)).sum
}

trait CommonPrefixDistance {
  def distance[T](a: Seq[T], b: Seq[T]): Double = {
    val commonPrefixLength = (a zip b).takeWhile(v => v._1 == v._2).length
    1 - commonPrefixLength / (min(a.length, b.length): Double)
  }
}

trait LetterPrefixDistance extends Similarity[String] with CommonPrefixDistance{
  override def distance(a: String, b: String): Double = distance(a.toSeq, b.toSeq)
}

trait WordPrefixDistance extends Similarity[IndexedSeq[String]] with CommonPrefixDistance{
  override def distance(a: IndexedSeq[String], b: IndexedSeq[String]): Double = distance[String](a, b)
}