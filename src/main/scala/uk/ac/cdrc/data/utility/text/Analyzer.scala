package uk.ac.cdrc.data.utility.text

import breeze.linalg.Counter
import uk.ac.cdrc.data.utility.text.entity.WordBag

import scala.util.matching.Regex

/**
  * Analyzers are used for creating intermediate data for searching efficiency
  */
trait Analyzer[T, U] {
  def process(e: T): U
}

trait AnalyzedPool[T, U] {
  self: Analyzer[T, U] =>
  val pool: IndexedSeq[T]
  lazy val processed: IndexedSeq[U] = for (e <- pool) yield process(e)
}

trait PreProcessor[U] extends Analyzer[String, U] {
  val inner: Analyzer[String, U]

  def preProcess(e: String): String
  override def process(e: String): U = inner.process(preProcess(e))
}

trait PunctuationRemoval[U] extends PreProcessor[U] {

  val punctuationPattern: Regex = "[,.']+".r

  override def preProcess(e: String): String = punctuationPattern replaceAllIn(e, "")
}

trait NumberRemoval[U] extends PreProcessor[U] with NumPatterns{
  override def preProcess(e: String): String =
    numPattern.replaceAllIn(numSpanPattern.replaceAllIn(e, " "), " ")

}


/**
  * Word bag based analyzer and pooled storage
  */
trait WordBagAnalyzer extends Analyzer[String, WordBag]{

  override def process(e: String): WordBag = WordBag(e)
}

trait WordBagAnalyzerWithoutNums
  extends NumberRemoval[WordBag]{
  override val inner = new WordBagAnalyzer{}
}

trait NormalizedWordBagAnalyzer
  extends CommonNormalizer[WordBag]{
  override val inner = new WordBagAnalyzerWithoutNums {}
}

class WordBagAnalyzedPool(override val pool: IndexedSeq[String])
  extends WordBagAnalyzerWithoutNums
    with AnalyzedPool[String, WordBag]

trait WordBagIDF
  extends Analyzer[String, WordBag]
    with AnalyzedPool[String, WordBag]{
  lazy val idf: Counter[String, Double] = Counter.count((for {
    wb <- processed
    word <- wb.keySet
  } yield word): _*).mapValues(x => 1 + math.log10(processed.length.toFloat / (x + 1)))
}


class WordBagAnalyzedPoolWithIDF(override val pool: IndexedSeq[String])
  extends WordBagIDF
    with NormalizedWordBagAnalyzer
/**
  * Word sequence based analyzer and pooled storage
  */
trait WordSeqAnalyzer extends Analyzer[String, IndexedSeq[String]] with NumPatterns {

  val tokenizer = DigitWordTokenizer

  override def process(e: String): IndexedSeq[String] = tokenizer.tokenize(
    numPattern.replaceAllIn(numSpanPattern.replaceAllIn(e, " "), " ")
  )

}

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
      numStart = m.group(1).toLong + 1
      numEnd = m.group(2).toLong - 1
      if numEnd - numStart < 3 && numEnd > numStart
      i <- numStart to numEnd
    } yield pos -> i.toString
    val nums = for {
      m <- (numPattern findAllIn s).matchData
      pos = m.start
    } yield pos -> m.toString
    val alphabets = for {
      ptn <- Seq(alphabetPattern, alphabetFlatPattern)
      m <- (ptn findAllIn s).matchData
    } yield (Character.getNumericValue(m.group(1).charAt(0)) - Character.getNumericValue('a') + 1).toString
    val allNums = (numSpan ++ nums).toVector
    alphabets.toVector ++ allNums.toSet.toVector.sortBy((x: (Int, String)) => x._1 -> x._2.toLong).map(_._2.toString)
  }
}

trait NormalizedNumberSpanAnalyzer
  extends CommonNormalizer[IndexedSeq[String]] {
  override val inner = new NumberSpanAnalyzer {}
}

class NumberSpanAnalyzedPool(override val pool: IndexedSeq[String])
  extends NormalizedNumberSpanAnalyzer
    with AnalyzedPool[String, IndexedSeq[String]]


