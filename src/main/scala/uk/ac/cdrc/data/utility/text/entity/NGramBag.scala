package uk.ac.cdrc.data.utility.text.entity

import breeze.linalg.{Counter, sum}
import uk.ac.cdrc.data.utility.text.{DigitWordTokenizer, Tokenizer}

/**
  * Created on 12/7/17.
  */
case class NGramBag(data: Counter[String, Int], n: Int) {

  def norm: Int = sum(data)

  def pnorm: Int = sum(data.values map {v: Int => if (v > 0) v else 0})

  def toNGramBag: NGramBag = this
}

object NGramBag {

  val tokenizer: Tokenizer = DigitWordTokenizer

  def apply(s: String): NGramBag = string2NGramBag(s)

  implicit def string2NGramBag(s: String, n: Int = 3): NGramBag = {
    val prep = tokenizer tokenize s mkString " "
    NGramBag(Counter.countTraversable(for (i <- 0 to (prep.length - n)) yield prep.substring(i, i + n)), n)
  }

  implicit def NGramBag2Counter(ng: NGramBag): Counter[String, Int] = ng.data
}
