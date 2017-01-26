package uk.ac.cdrc.data.utility.text
/**
  * Created  on 7/19/16.
  */

import uk.ac.cdrc.data.utility.text.entity._

trait SearchResult {
  def isEmpty: Boolean
}

case class SomeSearchResult(hits: IndexedSeq[(Int, Float)], items: IndexedSeq[String]) extends SearchResult {
  override def isEmpty = false
  def top: String = items(hits(0)._1)
  def multiTops: Boolean = if (hits.lengthCompare(2) < 0) false else hits(0)._2 == hits(1)._2
  def rank: IndexedSeq[String] = hits map {v => items(v._1)}
  def rankScore: IndexedSeq[(String, Float)] = hits map {v => (items(v._1), v._2)}
  def getMatching: Option[String] = if (!multiTops && hits(0)._2 < 100.0f) Some(top) else None
}

object EmptySearchResult extends SearchResult{
  override def isEmpty = true
}

trait Searcher{
  def search(query: String): SearchResult
}


case class IndexedSearcher(pool: Seq[String]) extends Searcher{
//  val tokenizer = SimpleTokenizer
//  private val items = pool.toArray
//  private val pairs = for (
//    i <- 0 to items.length;
//    w <- tokenizer tokenize items(i)
//  ) yield (w, i)

//  val indexible = pairs groupBy {_._1} map (d => (d._1, SparseVector(d._2, DenseVector.ones(d._2.length), items.length)))


  //TODO Make a faster search with the index
  override def search(query: String): SearchResult = EmptySearchResult

}

class WordBagSearcher(pool: Seq[String]) extends Searcher {
  private val items = (pool map (_.trim) filter (!_.isEmpty)).toArray

  val wordBags: IndexedSeq[WordBag] = items.indices map { i => WordBag(items(i))}

  def score(wb: WordBag, query: WordBag): Float = WordBagDistance.distance(wb, query)

  override def search(q: String): SearchResult = {
    val qwb = WordBag(q)
    val scores = wordBags.indices map (i => (i, score(wordBags(i), qwb)))

    SomeSearchResult(scores sortBy (v => (v._2, items(v._1).length)), items)
  }
}

case object EmptySearcher extends Searcher {
  override def search(q: String): SearchResult = EmptySearchResult
}

object WordBagSearcher {
  def apply() = EmptySearcher
  def apply(pool: Seq[String]): Searcher = if (pool.isEmpty) EmptySearcher else new WordBagSearcher(pool)
}

class AddressSearcher(pool: Seq[String]) extends Searcher with NumberSpanExtractor with WordStopper{
  private val items = (pool map (_.trim) filter (!_.isEmpty)).toArray

  val index: IndexedSeq[(WordBag, IndexedSeq[String], String)] = items.indices map { i =>
    (WordBag(items(i)), extract(items(i)), filter(items(i)))}

  def score(candidate: (WordBag, IndexedSeq[String], String), query: (WordBag, IndexedSeq[String], String)): Float ={
    val ws = SymmetricWordSetDistance.distance(candidate._1, query._1)
    val ns = NumbersOverlapDistance.distance(candidate._2, query._2)
    if (ns < 1f)
      ws * 10 + WordPrefixDistance.distance(candidate._3, query._3)
    else
      100f
  }

  override def search(q: String): SearchResult = {
    val fq = filter(q)
    val qwb = WordBag(q)
    val numSpan = extract(q)
    val scores = for {
      i <- index.indices
      s = score(index(i), (qwb, numSpan, fq))
    } yield (i, s)

    if (scores.isEmpty)
      EmptySearchResult
    else
      SomeSearchResult(scores sortBy (v => (v._2, items(v._1).length)), items)
  }
}

object AddressSearcher {
  def apply() = EmptySearcher
  def apply(pool: Seq[String]): Searcher = if (pool.isEmpty) EmptySearcher else new AddressSearcher(pool)
}
