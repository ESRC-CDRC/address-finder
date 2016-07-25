package uk.ac.cdrc.data.utility.text

import breeze.linalg.min

/**
  * Created  on 7/25/16.
  */
trait Similarity {
  def distance[T](a: IndexedSeq[T], b: IndexedSeq[T]): Float
}

object LevenshteinDistance extends Similarity {
  override def distance[T](a: IndexedSeq[T], b: IndexedSeq[T]): Float = {
    if (a.length == 0) b.length
    else if (b.length == 0) a.length
    else {
      val dist = Array.tabulate(a.length + 1, b.length + 1)((i, j) => if (i == 0 || j == 0) i + j else 0f)
      for (i <- 1 to a.length; j <- 1 to b.length) {
        dist(i)(j) = if (a(i-1) == b(j-1)) dist(i - 1)(j - 1) else min(dist(i - 1)(j), dist(i)(j - 1), dist(i - 1)(j - 1)) + 1
      }
      dist(a.length)(b.length)
    }
  }
}

