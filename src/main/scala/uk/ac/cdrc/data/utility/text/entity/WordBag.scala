package uk.ac.cdrc.data.utility.text.entity

import breeze.linalg.{Counter, sum}
import breeze.linalg.Counter.canMapValues
import uk.ac.cdrc.data.utility.text.DigitWordTokenizer

case class WordBag(data: Counter[String, Int]) {

  def norm: Int = sum(data)

  def pnorm: Int = sum(data.values map {v: Int => if (v > 0) v else 0})

  def toWordBag: WordBag = this
}

object WordBag {
  val tokenizer = DigitWordTokenizer

  def apply(s: String): WordBag = string2WordBag(s)

  implicit def string2WordBag(s: String): WordBag = {
    WordBag(Counter.countTraversable(tokenizer tokenize s))
  }

  implicit def counter2WordBag(c: Counter[String, Int]): WordBag = {
    WordBag(c)
  }

  implicit def wordBag2Counter(wb: WordBag): Counter[String, Int] = wb.data
}
