package uk.ac.cdrc.data.utility.text.entity

import breeze.linalg.{Counter, sum}
import breeze.linalg.Counter.canMapValues

case class Wordbag(val data: Counter[String, Int]) {

  def norm: Int = sum(data)

  def pnorm: Int = sum(data.values map {v: Int => if (v > 0) v else 0})

  def toWordbag: Wordbag = this
}

object Wordbag {
  val whiteSpace = "\\s+".r

  def apply(s: String) = string2Wordbag(s)

  implicit def string2Wordbag(s: String): Wordbag = {
    Wordbag(Counter.countTraversable(whiteSpace split s))
  }

  implicit def counter2Wordbag(c: Counter[String, Int]): Wordbag = {
    Wordbag(c)
  }

  implicit def wordbag2Counter(wb: Wordbag): Counter[String, Int] = wb.data

  def toWordbag(w: Wordbag): Wordbag = w
}
