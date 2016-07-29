package uk.ac.cdrc.data.utility.text
/**
  * Created  on 7/19/16.
  */

import uk.ac.cdrc.data.utility.text.entity._


case class SearchResult(hits: IndexedSeq[(Int, Float)], items: IndexedSeq[String]) {
  def top: String = {println(hits.length); items(hits(0)._1)}
  def multiTops: Boolean = if (hits.lengthCompare(2) < 0) false else hits(0)._2 == hits(1)._2
  def rank: IndexedSeq[String] = hits map {v => items(v._1)}
  def rankScore: IndexedSeq[(String, Float)] = hits map {v => (items(v._1), v._2)}
}

trait Searcher{
  def search(query: String): Option[SearchResult]
}


case class IndexedSearcher(pool: Seq[String]) extends Searcher{
  val tokenizer = SimpleTokenizer
  val items = pool.toArray
  val pairs = for (
    i <- 0 to items.length;
    w <- tokenizer tokenize items(i)
  ) yield (w, i)

//  val indexible = pairs groupBy {_._1} map (d => (d._1, SparseVector(d._2, DenseVector.ones(d._2.length), items.length)))


  //TODO Make a faster search with the index
  override def search(query: String): Option[SearchResult] = None

}

class WordBagSearcher(pool: Seq[String]) extends Searcher {
  val items = (pool map (_.trim) filter (!_.isEmpty)).toArray

  val wordBags: IndexedSeq[WordBag] = items.indices map { i => WordBag(items(i))}

  def score(wb: WordBag, query: WordBag): Float = WordBagDistance.distance(wb, query)

  override def search(q: String): Option[SearchResult] = {
    val qwb = WordBag(q)
    val scores = wordBags.indices map (i => (i, score(wordBags(i), qwb)))

    Some(SearchResult(scores sortBy (v => (v._2, items(v._1).length)), items))
  }
}

case object EmptySearcher extends Searcher {
  override def search(q: String): Option[SearchResult] = None
}

object WordBagSearcher {
  def apply() = EmptySearcher
  def apply(pool: Seq[String]) = if (pool.isEmpty) EmptySearcher else new WordBagSearcher(pool)
}

class AddressSearcher(pool: Seq[String]) extends Searcher with NumberSpanExtractor {
  val items = (pool map (_.trim) filter (!_.isEmpty)).toArray

  val index: IndexedSeq[(WordBag, IndexedSeq[String])] = items.indices map { i =>
    (WordBag(items(i)), extract(items(i)))}

  def score(candidate: (WordBag, IndexedSeq[String]), query: (WordBag, IndexedSeq[String])): Float ={
    val ws = WordSetDistance.distance(candidate._1, query._1)
    val ns = NumbersOverlapDistance.distance(candidate._2, query._2)
    if (ns < 1f)
      ws
    else
      100f
  }

  override def search(q: String): Option[SearchResult] = {
    val qwb = WordBag(q)
    val numSpan = extract(q)
    val scores = index.indices map (i => (i, score(index(i), (qwb, numSpan))))

    Some(SearchResult(scores sortBy (v => (v._2, items(v._1).length)), items))
  }
}

object AddressSearcher {
  def apply() = EmptySearcher
  def apply(pool: Seq[String]) = if (pool.isEmpty) EmptySearcher else new AddressSearcher(pool)
}
