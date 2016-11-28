package uk.ac.cdrc.data.utility.text

/**
  * Created  on 7/20/16.
  */


import org.scalatest._
import Inside._

class SearcherSpec extends FlatSpec with Matchers{

  "An default empty wordbag search" should "return nothing" in {
    val emptyPool = WordBagSearcher()
    val r = emptyPool search "a b c"
    r should matchPattern {case None => }
  }

  "An default empty AddressSearcher search" should "return nothing" in {
    val emptyPool = AddressSearcher()
    val r = emptyPool search "a b c"
    r should matchPattern {case None => }
  }

  "An empty wordbag search" should "return nothing" in {
    val emptyPool = WordBagSearcher(Seq[String]())
    val r = emptyPool search "a b c"
    r should matchPattern {case None => }
  }

  "An empty AddressSearcher search" should "return nothing" in {
    val emptyPool = AddressSearcher(Seq[String]())
    val r = emptyPool search "a b c"
    r should matchPattern {case None => }
  }

  "A wordbag searcher" should "find multi top matches" in {
    val s = WordBagSearcher(Array("a-b c d", "a b c", "c d"))
    val r = s search "a b c"
    inside(r) {
      case Some(rs) =>
        rs shouldBe 'multiTops
    }
    inside(r) {
      case Some(rs) =>
        rs.hits should have size 3
    }
  }

  "A searcher" should "not throw exception" in {
    val s = WordBagSearcher(Array("ggg  196  aaa  ccc ccc main  rrr  eee"))
    val r = s search "ggg 200 aaa  ccc  main  rrr"
    inside(r) {
      case Some(rs) =>
        rs.hits should have size 1
    }
  }

  "An AddressSearcher" should "not throw exception" in {
    val s = AddressSearcher(Array("ggg  196  aaa  ccc ccc main  rrr  eee"))
    val r = s search "ggg 20000000000 aaa  ccc  main  rrr"
    inside(r) {
      case Some(rs) =>
        rs.hits should have size 1
        rs.hits(0)._2 should be (100)
    }
  }

}
