package uk.ac.cdrc.data.utility.text

import uk.ac.cdrc.data.utility.text.entity.WordBag
import scala.util.matching.Regex

/**
  * Created  on 2/1/17.
  */
trait Analyzer[T, U] {
  def process(e: T): U
}

trait AnalyzedPool[T, U] {
  self: Analyzer[T, U] =>
  val pool: IndexedSeq[T]
  val processed: IndexedSeq[U] = for (e <- pool) yield process(e)
}

/**
  * Word bag based analyzer and pooled storage
  */
trait WordBagAnalyzer extends Analyzer[String, WordBag] {

  override def process(e: String): WordBag = WordBag(e)
}

class WordBagAnalyzedPool(override val pool: IndexedSeq[String]) extends WordBagAnalyzer with AnalyzedPool[String, WordBag]

/**
  * Word sequence based analyzer and pooled storage
  */
trait WordSeqAnalyzer extends Analyzer[String, IndexedSeq[String]] {

  val tokenizer = DigitWordTokenizer

  override def process(e: String): IndexedSeq[String] = tokenizer.tokenize(e)
}

class WordSeqAnalyzedPool(override val pool: IndexedSeq[String]) extends WordSeqAnalyzer with AnalyzedPool[String, IndexedSeq[String]]

/**
  * Number span analyzer and pooled storage
  */

trait NumberSpanAnalyzer extends Analyzer[String, IndexedSeq[String]] {

  val numSpanPattern: Regex = "(\\d+)\\s*-\\s*(\\d+)".r
  val numPattern: Regex = "\\d+".r

  override def process(s: String): IndexedSeq[String] = {
    val numSpan = for {
      m <- (numSpanPattern findAllIn s).matchData
      i <- m.group(1).toLong to m.group(2).toLong
    } yield i.toString
    val nums = for {
      m <- numPattern findAllIn (numSpanPattern replaceAllIn(s, " "))
    } yield m.toString
    (numSpan ++ nums).toIndexedSeq
  }
}

class NumberSpanAnalyzedPool(override val pool: IndexedSeq[String]) extends NumberSpanAnalyzer with AnalyzedPool[String, IndexedSeq[String]]
