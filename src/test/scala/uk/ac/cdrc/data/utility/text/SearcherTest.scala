package uk.ac.cdrc.data.utility.text

/**
  * Created  on 7/20/16.
  */


import org.scalatest._
import Inside._

class SearcherTestSpec extends FlatSpec with Matchers{
  val emptyPool = WordBagSearcher()

  "An empty wordbag search" should "return nothing" in {
    val r = (emptyPool search "a b c")
    r should matchPattern {case None => }
  }

  val s = WordBagSearcher(Array("a b c d", "a b c", "c d"))
  "A wordbag searcher" should "find multi top matches" in {
    val r = (s search "a b c")
    inside(r) {
      case Some(rs) =>
        rs shouldBe 'multiTops
    }
    inside(r) {
      case Some(rs) =>
        rs.hits should have size 3
    }

  }
}
