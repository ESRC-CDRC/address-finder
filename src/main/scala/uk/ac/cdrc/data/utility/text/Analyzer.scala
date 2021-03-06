package uk.ac.cdrc.data.utility.text

import breeze.linalg.Counter
import uk.ac.cdrc.data.utility.text.entity.{NGramBag, WordBag}

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

trait PreprocessedAnalyzer[U] extends Analyzer[String, U] {

  def preprocess(e: String): String = e
  abstract override def process(e: String): U = super.process(preprocess(e))
}

trait PunctuationRemoval[U] extends PreprocessedAnalyzer[U] {

  private val sepPunctuationPattern: Regex = "[,./]".r
  private val jointPunctuationPattern: Regex = "[']".r

  abstract override def preprocess(e: String): String =
    jointPunctuationPattern replaceAllIn (sepPunctuationPattern replaceAllIn(super.preprocess(e), " "), "")
}

trait NumberRemoval[U] extends PreprocessedAnalyzer[U] with NumPatterns{

  private val patterns = Seq(numSpanPattern, orderedNumPattern, numPattern)

  abstract override def preprocess(e: String): String =
    (super.preprocess(e) /: patterns) {case (x, ptn) => ptn.replaceAllIn(x, " ")}

}


/**
  * Word bag based analyzer and pooled storage
  */
trait WordBagAnalyzer extends Analyzer[String, WordBag]{

  override def process(e: String): WordBag = WordBag(e)
}

trait WordBagAnalyzerWithoutNums
  extends WordBagAnalyzer
  with NumberRemoval[WordBag]


trait NormalizedWordBagAnalyzer
  extends WordBagAnalyzerWithoutNums
    with PunctuationRemoval[WordBag]
    with TextNormalizer[WordBag]

class WordBagAnalyzedPool(override val pool: IndexedSeq[String])
  extends WordBagAnalyzerWithoutNums
    with AnalyzedPool[String, WordBag]


trait WordBagIDF
  extends Analyzer[String, WordBag]
    with AnalyzedPool[String, WordBag]{
  val globalDFR: Counter[String, Double]
  lazy val localDF: Counter[String, Double] = Counter.count((for {
    wb <- processed
    word <- wb.keySet
  } yield word): _*).mapValues(_.toDouble)

  lazy val idf: Counter[String, Double] = ((localDF * 0.5) + (globalDFR * (0.5 * processed.length))).
    mapValues(x => 1 + math.log10(processed.length.toFloat / (x + 1)))
}


class WordBagAnalyzedPoolWithIDF(override val pool: IndexedSeq[String], override val globalDFR: Counter[String, Double])
  extends WordBagIDF
    with NormalizedWordBagAnalyzer

/**
  * NGrams from strings
  */
trait NGramBagAnalyzer extends Analyzer[String, NGramBag]{

  override def process(e: String): NGramBag = NGramBag(e)
}

trait NGramBagAnalyzerWithoutNums
  extends NGramBagAnalyzer
    with NumberRemoval[NGramBag]

class NGramBagAnalyzedPool(override val pool: IndexedSeq[String])
  extends NGramBagAnalyzerWithoutNums
    with AnalyzedPool[String, NGramBag]

/**
  * Word sequence based analyzer and pooled storage
  */
trait WordSeqAnalyzer extends Analyzer[String, IndexedSeq[String]] {

  val tokenizer = DigitWordTokenizer

  def process(e: String): IndexedSeq[String] = tokenizer.tokenize(e)

}

class WordSeqAnalyzedPool(override val pool: IndexedSeq[String])
  extends WordSeqAnalyzer
    with NumberRemoval[IndexedSeq[String]]
    with TextNormalizer[IndexedSeq[String]]
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
      ptn <- Seq(alphabetSuffixPattern, alphabetFlatPattern)
      m <- (ptn findAllIn s).matchData
    } yield (Character.getNumericValue(m.group(1).charAt(0)) - Character.getNumericValue('a') + 1).toString
    val allNums = (numSpan ++ nums).toVector
    alphabets.toVector ++ allNums.toSet.toVector.sortBy((x: (Int, String)) => x._1 -> x._2.toLong).map(_._2.toString)
  }
}

/**
  * Number analyzer and pooled storage
  */
trait NumberAnalyzer extends Analyzer[String, IndexedSeq[String]] with NumPatterns{

  override def process(s: String): IndexedSeq[String] = {
    val nums = for {
      m <- (numPattern findAllIn s).matchData
      pos = m.start
    } yield pos -> m.toString
    val alphabets = for {
      ptn <- Seq(alphabetSuffixPattern, alphabetPattern)
      m <- (ptn findAllIn s).matchData
    } yield (Character.getNumericValue(m.group(1).charAt(0)) - Character.getNumericValue('a') + 1).toString
    alphabets.toVector ++ nums.toSet.toVector.sortBy((x: (Int, String)) => x._1 -> x._2.toLong).map(_._2.toString)
  }
}
class NumberSpanAnalyzedPool(override val pool: IndexedSeq[String])
    extends NumberSpanAnalyzer
      with PunctuationRemoval[IndexedSeq[String]]
      with NumberNormalizer[IndexedSeq[String]]
      with AnalyzedPool[String, IndexedSeq[String]]

class NumberAnalyzedPool(override val pool: IndexedSeq[String])
  extends NumberAnalyzer
    with PunctuationRemoval[IndexedSeq[String]]
    with NumberNormalizer[IndexedSeq[String]]
    with AnalyzedPool[String, IndexedSeq[String]]
