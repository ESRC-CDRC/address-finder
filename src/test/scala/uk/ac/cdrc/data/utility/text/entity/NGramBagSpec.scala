package uk.ac.cdrc.data.utility.text.entity

import org.scalatest.{FlatSpec, Matchers}
import uk.ac.cdrc.data.utility.text.entity.NGramBag._
/**
  * Created on 12/7/17.
  */
class NGramBagSpec extends FlatSpec with Matchers{

  "A NGramBag" should "process string" in {
    val ng = string2NGramBag("this is a test")
    ng.data.data should contain ("thi" -> 1)
    ng.data.data should contain ("is " -> 2)
  }
}
