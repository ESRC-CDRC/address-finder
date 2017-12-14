/**
  * The searchers are the core of the package.
  */
package uk.ac.cdrc.data.utility.text

import breeze.linalg.Counter
import uk.ac.cdrc.data.utility.text.entity._

/**
  * The search results are the returns from the searchers
  * It defines several utility functions such as mutltiTops which indicate
  * whether a single most close item can be found. If not it usually means
  * there are missing information in either of the query or the collection.
  * @param hits the scored items in (index, score) pairs, smaller scores mean closer matches
  * @param items the items in the collection the score of which are recorded in the hits
  */
case class SearchResult(hits: Seq[(Int, Double)], scoreLimit: Double = Double.MaxValue)
                       (implicit items: IndexedSeq[String]) {
  val orderedHits: IndexedSeq[(Int, Double)] = hits.sortBy(_._2).toIndexedSeq

  /**
    * Get the top match
    * @return the item got matched
    */
  def top: String = items(orderedHits.head._1)

  /**
    * Whether their are more than one items having the smallest score in the collection
    * @return true if no single closest matches
    */
  def multiTops: Boolean = if (orderedHits.lengthCompare(2) < 0) false else orderedHits(0)._2 == orderedHits(1)._2

  /**
    * Get the items ranked by their scores
    * @return the items list in the order of their scores from closest to the farthest
    */
  def rank: IndexedSeq[String] = orderedHits map {v => items(v._1)}

  /**
    * Get the items and the score in ranking order
    * @return the list of (item, score) pairs
    */
  def rankScore: IndexedSeq[(String, Double)] = orderedHits map {v => (items(v._1), v._2)}

  /**
    * Get the only match if there is one
    * @return the only single match
    */
  def matched: Option[String] = if (!multiTops && orderedHits.head._2 < scoreLimit) Some(top) else None

  /**
    * Get the only match if there is one
    * @return the only single match with its index in the original pool
    */
  def matchedWithIndex: Option[(String, Int)] = if (!multiTops && orderedHits.head._2 < scoreLimit) Some((top, orderedHits.head._1)) else None

  /**
    * Remove the top matching
    * @return a new result set by removing the top matched
    */
  def pop: Option[SearchResult] = if(orderedHits.length > 1) Some(SearchResult(orderedHits.tail, scoreLimit)) else None
}


trait Searcher {
  val pool: IndexedSeq[String]

  /**
    * The max score for a candidate to be accepted as a match
    */
  val matchScoreLimit: Double = Double.MaxValue
  /**
    * Search the stored collection for the given query
    * @param q a simple string query
    * @return a possible findings in the collection
    */
  def search(q: String): Option[SearchResult]
}

/**
  * A searcher with preporcessed information before querying enabled by AnalyzedPool
  * @tparam U the type of preprocessed items
  */
trait PreProcessingSearcher[U] extends Searcher {
  self: Analyzer[String, U] with AnalyzedPool[String, U] with Similarity[U] =>

  override def search(q: String): Option[SearchResult] = {
    val processedQuery = process(q)
    val scores = for {
      (u, i) <- processed.zipWithIndex
    } yield (i, distance(u, processedQuery))

    Some(SearchResult(scores, matchScoreLimit)(pool))
  }
}

class PooledSearcher(implicit override val pool: IndexedSeq[String]) extends Searcher {
  self: Similarity[String] =>

  override def search(q: String): Option[SearchResult] = {
    val scores = for {
      (u, i) <- pool.zipWithIndex
    } yield (i, distance(u, q))

    Some(SearchResult(scores, matchScoreLimit)(pool))
  }
}

/**
  * A searcher with nothing in the collection
  */
case object EmptySearcher extends Searcher {
  override def search(q: String): Option[SearchResult] = None

  override val pool: IndexedSeq[String] = IndexedSeq.empty
}

/**
  * A word bag based searcher
  * @param pool a pool of strings
  */
class WordBagSearcher(implicit override val pool: IndexedSeq[String]) extends WordBagAnalyzedPool(pool)
  with WordBagAnalyzer
  with PreProcessingSearcher[WordBag]
  with WordBagDistance


object WordBagSearcher {
  def apply() = EmptySearcher
  def apply(implicit pool: IndexedSeq[String]): Searcher = if (pool.isEmpty) EmptySearcher else new WordBagSearcher
}

/**
  * A ngram bag based searcher
  * @param pool a pool of strings
  */
class NGramBagSearcher(implicit override val pool: IndexedSeq[String]) extends NGramBagAnalyzedPool(pool)
  with NGramBagAnalyzer
  with PreProcessingSearcher[NGramBag]
  with NGramBagDistance


object NGramBagSearcher {
  def apply() = EmptySearcher
  def apply(implicit pool: IndexedSeq[String]): Searcher = if (pool.isEmpty) EmptySearcher else new NGramBagSearcher
}
/**
  * A searcher that combines results from a set of searchers
  * @param searchers a list of searchers with the same collection
  * @param weights a list of numbers deciding the weights for each searcher
  * @param matchScoreLimit same as [[uk.ac.cdrc.data.utility.text.Searcher#matchScoreLimit]]
  * @param pool the collection
  */
class CompositeSearcher(searchers: Seq[Searcher], weights: Seq[Double], override val matchScoreLimit: Double)
                       (implicit override val pool: IndexedSeq[String]) extends Searcher {

  /**
    * The score is a linear combination of the scores from all the sub searchers via the given weights
    * @param q a simple string query
    * @return a possible findings in the collection
    */
  override def search(q: String): Option[SearchResult] = {
    val scoreParts = for {
      (searcher, weight) <- searchers zip weights
      res: SearchResult <- searcher.search(q).toSet
      (i, score) <- res.hits
    } yield (i, score * weight)
    if(scoreParts.isEmpty)
      None
    else {
      val scores = scoreParts.groupBy(_._1).mapValues{(x: Seq[(Int, Double)]) => x.map(_._2).sum}.toSeq
      Some(SearchResult(scores, matchScoreLimit))
    }
  }
}

/**
  * A predefined address searcher that should be used
  * @param pool a set of addresses in string
  * @param globalDFR A Counter object specifying the global docFreq (0<=df<=1) for each term
  */
class AddressSearcher(implicit override val pool: IndexedSeq[String], globalDFR: Counter[String, Double]=Counter[String, Double]()) extends Searcher {

  val searchers: Seq[Searcher] = Seq(
    new NumberAnalyzedPool(pool) with PreProcessingSearcher[IndexedSeq[String]] with StrictNumberOverlapDistance,
    new WordBagAnalyzedPoolWithIDF(pool, globalDFR) with PreProcessingSearcher[WordBag] with WordSetDistanceWithIDF,
    new PooledSearcher with WeightedLevenshteinStringDistance
  )

  val weights: Seq[Double] = Seq(100, 10, 16)

  val comboSearcher = new CompositeSearcher(searchers, weights, 100)

  override def search(q: String): Option[SearchResult] = comboSearcher.search(q)
}

object AddressSearcher {
  def apply() = EmptySearcher
  def apply(implicit pool: IndexedSeq[String], globalDFR: Counter[String, Double]=Counter[String, Double]()): Searcher = if (pool.isEmpty) EmptySearcher else new AddressSearcher
}
