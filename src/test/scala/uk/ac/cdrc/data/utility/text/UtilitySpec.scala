package uk.ac.cdrc.data.utility.text

import org.scalatest.{FlatSpec, Matchers}
import uk.ac.cdrc.data.utility.text.Utility.condenseAddresses
/**
  * Created on 7/19/17.
  */
class UtilitySpec extends FlatSpec with Matchers{

  "Utility.condense" should "join similar addresses" in {
    val addrBase = IndexedSeq(
      "3 aaabbbs road" -> 10l,
      "1 aaabbb road" -> 11l,
      "1 ccc aaabbb road" -> 12l,
      "1 ddd aaabbb road" -> 12l,
      "1 eee aaabbb road" -> 12l,
      "2 aaabbbs road" -> 13l
    )
    val condensed = condenseAddresses(addrBase)
    condensed(1)._2 should be (condensed(2)._2)
    condensed(2)._2 should be (condensed(3)._2)
    condensed(3)._2 should be (condensed(4)._2)
    condensed(0)._2 shouldNot be (condensed(4)._2)
    condensed(5)._2 shouldNot be (condensed(4)._2)
    condensed(5)._2 shouldNot be (condensed(0)._2)
  }
}
