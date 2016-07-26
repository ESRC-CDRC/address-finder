package uk.ac.cdrc.data.utility.text.entity

import breeze.linalg.{Counter, sum}
import breeze.linalg.Counter.canMapValues
import breeze.storage.Zero


class Wordbag (override val data : scala.collection.mutable.Map[String,Int]) (implicit zero : Zero[Int]) extends Counter[String,Int] {

  def default = zero.zero

  def norm: Int = Wordbag.norm(this)

  def pnorm: Int = Wordbag.pnorm(this)

}

object Wordbag {
  val whiteSpace = "\\s+".r

  def apply(s: String) = string2Wordbag(s)

  implicit def string2Wordbag(s: String): Wordbag = {
    new Wordbag(Counter.countTraversable(whiteSpace split s).data)
  }

  def norm(c: Counter[String, Int]): Int = sum(c)

  def pnorm(c: Counter[String, Int]): Int = sum(c.values map {v: Int => if (v > 0) v else 0})

  def diff(a: Counter[String, Int], b: Counter[String, Int]): Counter[String, Int] = a - b

  def toWordbag(w: Wordbag): Wordbag = w
}
