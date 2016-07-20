package uk.ac.cdrc.data.utility.text
/**
  * Created  on 7/19/16.
  */

import breeze.linalg.{Counter, DenseVector, SparseVector, VectorBuilder, sum}

class SearchResult(val hits: IndexedSeq[(Int, Int)], val items: IndexedSeq[String]) {
  def top: String = items(hits(0)._1)
  def multiTops: Boolean = hits(0)._2 == hits(1)._2
  def rank: IndexedSeq[String] = hits map {v => items(v._1)}
  def rankScore: IndexedSeq[(String, Int)] = hits map {v => (items(v._1), v._2)}
}

trait Searcher {
  val sepChar = "\\s+".r
  def search(q: String): SearchResult
}

class IndexedSearcher(pool: Seq[String]) extends Searcher{

  val items = pool.toArray
  val pairs = for (
    i <- 0 to items.length;
    w <- sepChar split items(i)
  ) yield (w, i)

  val indexible = pairs groupBy {_._1} map (d => (d._1, SparseVector(d._2, DenseVector.ones(d._2.length), items.length)))


  //TODO Make a faster search with the index
  override def search(query: String): SearchResult = ???

}


class WordBag

class WordBagMatcher(pool: Seq[String]) extends Searcher {
  val items = pool.toArray

  val wordBags: IndexedSeq[Counter[String, Int]] = 0 to items.length map {i => mkWordBag(items(i))}

  def mkWordBag(s: String) = Counter.countTraversable(sepChar split s)

  override def search(q: String): SearchResult = {
    val qb = mkWordBag(q)
    val distances = 0 to wordBags.length map (i => (i, sum(wordBags(i) - qb)))
    new SearchResult(distances, items)
  }
}
