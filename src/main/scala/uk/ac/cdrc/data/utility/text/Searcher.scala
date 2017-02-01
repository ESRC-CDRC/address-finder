package uk.ac.cdrc.data.utility.text
/**
  * Created  on 7/19/16.
  */

import uk.ac.cdrc.data.utility.text.entity._

case class SearchResult(hits: Seq[(Int, Float)])(implicit items: IndexedSeq[String]) {
  val orderedHits: IndexedSeq[(Int, Float)] = hits.sortBy(_._2).toIndexedSeq
  def top: String = items(orderedHits.head._1)
  def multiTops: Boolean = if (orderedHits.lengthCompare(2) < 0) false else orderedHits(0)._2 == orderedHits(1)._2
  def rank: IndexedSeq[String] = orderedHits map {v => items(v._1)}
  def rankScore: IndexedSeq[(String, Float)] = orderedHits map {v => (items(v._1), v._2)}
  def getMatching: Option[String] = if (!multiTops && orderedHits.head._2 < 100.0f) Some(top) else None
}


trait Searcher {
  val pool: IndexedSeq[String]
  def search(q: String): Option[SearchResult]
}

trait PooledSearcher[U] extends Searcher {
  self: Analyzer[String, U] with AnalyzedPool[String, U] with Similarity[U] =>
  override def search(q: String): Option[SearchResult] = {
    val qwb = process(q)
    val scores = for {
      i <- processed.indices
      u = processed(i)
    } yield (i, distance(u, qwb))

    Some(SearchResult(scores)(pool))
  }
}

case object EmptySearcher extends Searcher {
  override def search(q: String): Option[SearchResult] = None

  override implicit val pool: IndexedSeq[String] = IndexedSeq.empty
}

class WordBagSearcher(override val pool: IndexedSeq[String]) extends WordBagAnalyzedPool(pool)
  with WordBagAnalyzer
  with PooledSearcher[WordBag]
  with WordBagDistance


object WordBagSearcher {
  def apply() = EmptySearcher
  def apply(pool: IndexedSeq[String]): Searcher = if (pool.isEmpty) EmptySearcher else new WordBagSearcher(pool)
}

class CompositeSearcher(searchers: Seq[Searcher], weights: Seq[Float])
                       (override implicit val pool: IndexedSeq[String]) extends Searcher {

  override def search(q: String): Option[SearchResult] = {
    val scoreParts = for {
      (searcher, weight) <- searchers zip weights
      res: SearchResult <- searcher.search(q).toSet
      (i, score) <- res.hits
    } yield (i, score * weight)
    if(scoreParts.isEmpty)
      None
    else
      Some(SearchResult(scoreParts.groupBy(_._1).mapValues{(x: Seq[(Int, Float)]) => x.map(_._2).sum}.toSeq))
  }
}

class AddressSearcher(override val pool: IndexedSeq[String]) extends Searcher {

  val searchers: Seq[Searcher] = Seq(
    new NumberSpanAnalyzedPool(pool) with PooledSearcher[IndexedSeq[String]] with StrictNumberOverlapDistance,
    new WordBagAnalyzedPool(pool) with PooledSearcher[WordBag] with SymmetricWordSetDistance,
    new WordSeqAnalyzedPool(pool) with PooledSearcher[IndexedSeq[String]] with WordPrefixDistance
  )

  val weights: Seq[Float] = Seq(100, 10, 1)

  val comboSearcher = new CompositeSearcher(searchers, weights)(pool)

  override def search(q: String): Option[SearchResult] = comboSearcher.search(q)
}

object AddressSearcher {
  def apply() = EmptySearcher
  def apply(pool: IndexedSeq[String]): Searcher = if (pool.isEmpty) EmptySearcher else new AddressSearcher(pool)
}
