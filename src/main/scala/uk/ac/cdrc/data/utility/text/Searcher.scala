package uk.ac.cdrc.data.utility.text
/**
  * Created  on 7/19/16.
  */

import breeze.linalg.{Counter, DenseVector, SparseVector, VectorBuilder, sum}
import breeze.linalg.Counter.canIterateValues


class SearchResult(val hits: IndexedSeq[(Int, Float)], val items: IndexedSeq[String]) {
  def top: String = items(hits(0)._1)
  def multiTops: Boolean = hits(0)._2 == hits(1)._2
  def rank: IndexedSeq[String] = hits map {v => items(v._1)}
  def rankScore: IndexedSeq[(String, Float)] = hits map {v => (items(v._1), v._2)}
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

//  val indexible = pairs groupBy {_._1} map (d => (d._1, SparseVector(d._2, DenseVector.ones(d._2.length), items.length)))


  //TODO Make a faster search with the index
  override def search(query: String): SearchResult = ???

}


class WordBag

class WordBagSearcher(pool: Seq[String]) extends Searcher {

  type WordBag = Counter[String, Int]

  val items = pool.toArray

  val wordBags: IndexedSeq[WordBag] = items.indices map {i => mkWordBag(items(i))}

  def mkWordBag(s: String) = Counter.countTraversable(sepChar split s)

  def score(wb: WordBag, qwb: WordBag): Float = sum((qwb - wb).values map (v => if (v > 0) v else 0))

  override def search(q: String): SearchResult = {
    val qwb = mkWordBag(q)
    val distances = wordBags.indices map (i => (i, score(wordBags(i), qwb)))

    val scores = distances sortWith {_._2 < _._2}
    new SearchResult(scores, items)
  }
}
