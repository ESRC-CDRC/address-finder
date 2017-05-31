/**
  * Similarities are defined as distances between candidates. That is similar items will result in small number.
  */
package uk.ac.cdrc.data.utility.text

import breeze.linalg.{DenseVector, argmin, max, min}
import breeze.numerics.exp
import uk.ac.cdrc.data.utility.text.entity.WordBag
import uk.ac.cdrc.data.utility.text.entity.WordBag._

/**
  * The similarity interface which allows overriding the distance method
  * to assign a real number for a -> b
  * @tparam T The type of objects that going to be compared
  */
trait Similarity[T] {
  def distance(a: T, b: T): Double
}

/**
  * The Levenshtein distance which is also called edit distance.
  * This implementation is not efficient enough for millions of rows.
  */
trait LevenshteinDistance[T <: IndexedSeq[_]] {

  def edit_distance(a: T, b: T): Double = {
    if (a.isEmpty) b.length
    else if (b.isEmpty) a.length
    else {
      val dist = Array.tabulate(a.length + 1, b.length + 1)((i, j) => if (i == 0 || j == 0) i + j else 0f)
      for (i <- 1 to a.length; j <- 1 to b.length) {
        dist(i)(j) =
          if (a(i-1) == b(j-1)) dist(i - 1)(j - 1)
          else min(dist(i - 1)(j), dist(i)(j - 1), dist(i - 1)(j - 1)) + 1
      }
      dist(a.length)(b.length)
    }
  }
}

/**
  * Emphasize the difference at the beginning of the two strings
  */
trait WeightedLevenshteinDistance[U, T <: IndexedSeq[U]] {

  def weight(charA: U, charB: U, pos: Double, op: Int): Double = exp(-pos/2.0d) * (1 - op)

  def edit_distance(a: T, b: T): Double = {
    val refLen: Double = min(a.length, b.length)
    if (a.isEmpty) b.length
    else if (b.isEmpty) a.length
    else {
      val dist = Array.tabulate(a.length + 1, b.length + 1)((i, j) => if (i == 0 || j == 0) i + j else 0d)
      for (i <- 1 to a.length; j <- 1 to b.length) {
        dist(i)(j) =
          if (a(i-1) == b(j-1)) dist(i - 1)(j - 1)
          else {
            val choices = DenseVector(dist(i - 1)(j), // insert
                                      dist(i)(j - 1)) // delete
            val op = argmin(choices)
            weight(a(i-1), b(j-1), min(i, j).toDouble / refLen, op) + choices(op)
          }
      }
      dist(a.length)(b.length)
    }
  }
}

trait LevenshteinStringDistance
  extends Similarity[String]
    with LevenshteinDistance[Vector[Char]] {
  def distance(a: String, b: String): Double = edit_distance(a.toVector, b.toVector)
}

trait LevenshteinWordSeqDistance
  extends Similarity[IndexedSeq[String]]
    with LevenshteinDistance[IndexedSeq[String]] {
  def distance(a: IndexedSeq[String], b: IndexedSeq[String]): Double = edit_distance(a, b)
}

trait WeightedLevenshteinStringDistance
  extends Similarity[String]
    with WeightedLevenshteinDistance[Char, Vector[Char]] {
  def distance(a: String, b: String): Double = edit_distance(a.toVector, b.toVector)
}
/**
  * Defines the distance between two set of numbers in addresses
  * emphasizing on whether the number appearing in the target (b)
  */
trait NumberSpanDistance extends Similarity[IndexedSeq[String]] {

  def distance(a: IndexedSeq[String], b: IndexedSeq[String]): Double = {
    if (b.isEmpty)
      0.0d
    else
      (b.toSet -- a.toSet).size
  }
}

/**
  * Similar to the span distance including mutual difference
  */
trait NumberOverlapDistance extends Similarity[IndexedSeq[String]]{

  def distance(a: IndexedSeq[String], b: IndexedSeq[String]): Double = {
    val aSet = a.toSet
    val bSet = b.toSet
    val union: Double = (bSet | aSet).size
    if (union == 0)
      0.0d
    else
      1.0d - (bSet & aSet).size / union
  }
}

/**
  * Similar to the span distance including mutual difference
  */
trait NumberSeqDistance extends Similarity[IndexedSeq[String]] with CommonPrefixDistance{
  override def distance(a: IndexedSeq[String], b: IndexedSeq[String]): Double =
    if (a.isEmpty && b.isEmpty)
      0d
    else
      super[CommonPrefixDistance].distance(a.reverse, b.reverse)

}

/**
  * Binarise the overlaping distance
  */
trait StrictNumberOverlapDistance extends NumberOverlapDistance with NumberSeqDistance{
  override def distance(a: IndexedSeq[String], b: IndexedSeq[String]): Double =
    super[NumberOverlapDistance].distance(a, b) + super[NumberSeqDistance].distance(a, b)
}

/**
  * Word vector difference
  */
trait WordBagDistance extends Similarity[WordBag] {
  def distance(a: WordBag, b: WordBag): Double = (wordBag2Counter(b) - wordBag2Counter(a)).pnorm
}

/**
  * Word set difference
  */
trait WordSetDistance extends Similarity[WordBag] {
  def distance(a: WordBag, b: WordBag): Double = (b.keySet -- a.keySet).size / b.size.toDouble
}

/**
  * Weight the difference by their positions in the string
  */
trait PriorityWordDistance extends Similarity[IndexedSeq[String]] {
  def posWeight[T](seq: Seq[T], w: T): Double = {
    val p = seq.indexOf(w)
    if (p >= 0) (seq.length - p) / seq.length
    else 0.0f
  }
  override def distance(a: IndexedSeq[String], b: IndexedSeq[String]): Double = {
    (for {
      w <- b.toSet -- a.toSet
    } yield posWeight(a, w) + posWeight(b, w)).sum
  }
}

/**
  * Word set difference with symmetry
  */
trait SymmetricWordSetDistance extends Similarity[WordBag] {
  def distance(a: WordBag, b: WordBag): Double =
    ((b.keySet -- a.keySet).size + (a.keySet -- b.keySet).size) / (a.size + b.size).toDouble
}

/**
  * Using IDF to weight the difference
  */
trait SymmetricWordSetDistanceWithIDF extends Similarity[WordBag] {
  self: WordBagAnalyzedPoolWithIDF =>
  def distance(a: WordBag, b: WordBag): Double = (for {
    word <- (b.keySet -- a.keySet) ++ (a.keySet -- b.keySet)
  } yield idf(word)).sum
}

/**
  * Using IDF to weight the single side difference
  */
trait WordSetDistanceWithIDF extends Similarity[WordBag] {
  self: WordBagAnalyzedPoolWithIDF =>
  def distance(a: WordBag, b: WordBag): Double = (for {
    word <- b.keySet -- a.keySet
  } yield idf(word)).sum
}

/**
  * The distance determined by the shared prefix, may not be very useful
  */
trait CommonPrefixDistance {
  def distance[T](a: Seq[T], b: Seq[T]): Double = {
    val commonPrefixLength = (a zip b).takeWhile(v => v._1 == v._2).length
    1 - commonPrefixLength / max(a.length, b.length).toDouble
  }
}

/**
  * The letter-wise distance for shared prefix
  */
trait LetterPrefixDistance extends Similarity[String] with CommonPrefixDistance{
  override def distance(a: String, b: String): Double = distance(a.toSeq, b.toSeq)
}

/**
  * The word-wise distance for shared prefix
  */
trait WordPrefixDistance extends Similarity[IndexedSeq[String]] with CommonPrefixDistance{
  override def distance(a: IndexedSeq[String], b: IndexedSeq[String]): Double = distance[String](a, b)
}