package uk.ac.cdrc.data.utility.text

import breeze.linalg.Counter
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
trait WordBagAnalyzer extends Analyzer[String, WordBag] with NumPatterns{

  override def process(e: String): WordBag = WordBag(
    numPattern.replaceAllIn(numSpanPattern.replaceAllIn(e, " "), " ")
  )
}

object WordBagAnalyzer extends WordBagAnalyzer

class WordBagAnalyzedPool(override val pool: IndexedSeq[String])
  extends WordBagAnalyzer
    with AnalyzedPool[String, WordBag]

class WordBagAnalyzedPoolWithIDF(override val pool: IndexedSeq[String])
  extends PunctuationRemoval[WordBag, WordBagAnalyzer](WordBagAnalyzer)
    with AnalyzedPool[String, WordBag]{
  val idf: Counter[String, Double] = Counter.count((for {
    wb <- processed
    word <- wb.keySet
  } yield word): _*).mapValues(x => 1 + math.log10(processed.length.toFloat / (x + 1)))
}

/**
  * Word sequence based analyzer and pooled storage
  */
trait WordSeqAnalyzer extends Analyzer[String, IndexedSeq[String]] with NumPatterns {

  val tokenizer = DigitWordTokenizer

  override def process(e: String): IndexedSeq[String] = tokenizer.tokenize(
    numPattern.replaceAllIn(numSpanPattern.replaceAllIn(e, " "), " ")
  )

}

object WordSeqAnalyzer extends WordSeqAnalyzer

class WordSeqAnalyzedPool(override val pool: IndexedSeq[String])
  extends WordSeqAnalyzer
    with AnalyzedPool[String, IndexedSeq[String]]

/**
  * Number span analyzer and pooled storage
  */
trait NumberSpanAnalyzer extends Analyzer[String, IndexedSeq[String]] with NumPatterns{

  override def process(s: String): IndexedSeq[String] = {
    val numSpan = for {
      m <- (numSpanPattern findAllIn s).matchData
      pos = m.start
      i <- m.group(1).toLong to m.group(2).toLong
    } yield pos -> i.toString
    val nums = for {
      m <- (numPattern findAllIn (numSpanPattern replaceAllIn(s, m => " " * (m.end - m.start)))).matchData
      pos = m.start
    } yield pos -> m.toString
    (numSpan ++ nums).toVector.sortBy(x => x._1 -> x._2.toLong).map(_._2.toString)
  }
}

object NumberSpanAnalyzer extends NumberSpanAnalyzer

class NumberSpanAnalyzedPool(override val pool: IndexedSeq[String])
  extends NumberSpanAnalyzer
    with AnalyzedPool[String, IndexedSeq[String]]


trait NestedAnalyzer[U, A <: Analyzer[String, U]] extends Analyzer[String, U]{
  val inner: A

  def preProcess(e: String): String
  override def process(e: String): U = inner.process(preProcess(e))
}

class PunctuationRemoval[U, A <: Analyzer[String, U]](override val inner: A) extends NestedAnalyzer[U, A] {
  val punctuationPattern: Regex = "[,.']+".r

  override def preProcess(e: String): String = punctuationPattern replaceAllIn(e, "")
}
