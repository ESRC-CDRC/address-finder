package uk.ac.cdrc.data.utility.text

/**
  * Created  on 7/20/16.
  */


import org.scalatest.Inside._
import org.scalatest._

class SearcherSpec extends FlatSpec with Matchers{

  "An default empty wordbag search" should "return nothing" in {
    val emptyPool = WordBagSearcher()
    val r = emptyPool search "a b c"
    r should be (None)
  }

  "An default empty AddressSearcher search" should "return nothing" in {
    val emptyPool = AddressSearcher()
    val r = emptyPool search "a b c"
    r should be (None)
  }

  "An empty wordbag search" should "return nothing" in {
    val emptyPool = WordBagSearcher(IndexedSeq[String]())
    val r = emptyPool search "a b c"
    r should be (None)
  }

  "An empty AddressSearcher search" should "return nothing" in {
    val emptyPool = AddressSearcher(IndexedSeq[String]())
    val r = emptyPool search "a b c"
    r should be (None)
  }

  "A wordbag searcher" should "find multi top matches" in {
    val s = WordBagSearcher(IndexedSeq("a-b c d", "a b c", "c d"))
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
    val s = WordBagSearcher(IndexedSeq("ggg  196  aaa  ccc ccc main  rrr  eee"))
    val r = s search "ggg 200 aaa  ccc  main  rrr"
    inside(r) {
      case Some(rs) =>
        rs.hits should have size 1
    }
  }

  "An AddressSearcher" should "deal with single candidate" in {
    val s = AddressSearcher(IndexedSeq("ggg  196  aaa  ccc ccc main  rrr  eee"))
    val r = s search "ggg 20000000000 aaa  ccc  main  rrr"
    inside(r) {
      case Some(rs) =>
        rs.hits should have size 1
        rs.hits.head._2 should be > 100d
    }
  }

  it should "deal with two candidates" in {
    val s = AddressSearcher(IndexedSeq(
      "ggg  196  aaa  ccc ccc main  rrr  eee",
      "ggg  197  aaa  ccc ccc main  rrr  eee"
    ))
    val r = s search "ggg 20000000000 aaa  ccc  main  rrr"
    inside(r) {
      case Some(rs) =>
        rs.hits should have size 2
        rs.hits.head._2 should be > 100d
        rs.getMatching should be (None)
        rs shouldBe 'multiTops
    }
  }

  it should "deal with suffixed candidates" in {
    val s = AddressSearcher(IndexedSeq(
      "ggg  196a  aaa  ccc ccc main  rrr  eee",
      "ggg  196  aaa  ccc ccc main  rrr  eee",
      "ggg  197  aaa  ccc ccc main  rrr  eee"
    ))
    val r = s search "ggg 196 aaa  ccc  main  rrr"
    inside(r) {
      case Some(rs) =>
        rs.hits should have size 3
        rs should not be 'multiTops
    }
  }

  it should "deal with difficult suffixed candidates" in {
    val addrs = IndexedSeq(
      "ggg  196a  aaa  ccc ccc main  rrr  eee",
      "ggg  196  aaa  ccc ccc main  rrr  eee",
      "ggg  197  aaa  ccc ccc main  rrr  eee",
      "ggg  197a  aaa  ccc ccc main  rrr  eee"
    )
    val s = AddressSearcher(addrs)
    val r = s search "ggg 196 aaa  ccc  main  rrr"
    inside(r) {
      case Some(rs) =>
        rs should not be 'multiTops
        rs.top should be (addrs(1))
    }
  }

  it should "deal with hard suffixed candidates" in {
    val addrs = IndexedSeq(
      "flat 4 5 ggg  aaa  ccc ccc main  rrr  eee",
      "flat 5 4 ggg  aaa  ccc ccc main  rrr  eee",
      "flat 6 7 ggg  aaa  ccc ccc main  rrr  eee"
    )
    val s = AddressSearcher(addrs)
    val r = s search "Flat 4 5 ggg aaa  ccc  main  rrr"
    inside(r) {
      case Some(rs) =>
        rs should not be 'multiTops
        rs.top should be (addrs.head)
    }
  }

  it should "deal with harder suffixed candidates" in {
    val addrs = IndexedSeq(
      "flat a 5 ggg  aaa  ccc ccc main  rrr  eee",
      "flat a 4 ggg  aaa  ccc ccc main  rrr  eee",
      "flat b 4 ggg  aaa  ccc ccc main  rrr  eee",
      "5 ggg  aaa  ccc ccc main  rrr  eee"
    )
    val s = AddressSearcher(addrs)
    val r = s search "5a ggg aaa  ccc  main  rrr"
    inside(r) {
      case Some(rs) =>
        rs should not be 'multiTops
        rs.top should be (addrs.head)
    }
  }

  it should "deal with number span candidates" in {
    val addrs = IndexedSeq(
      "3-5 ggg  aaa  ccc ccc main  rrr  eee",
      "6 ggg  aaa  ccc ccc main  rrr  eee",
      "7 ggg  aaa  ccc ccc main  rrr  eee",
      "8 ggg  aaa  ccc ccc main  rrr  eee"
    )
    val s = AddressSearcher(addrs)
    val r = s search "5 ggg aaa  ccc  main  rrr"
    inside(r) {
      case Some(rs) =>
        rs should not be 'multiTops
        rs.top should be (addrs.head)
    }
  }

  it should "deal with another number span candidates" in {
    val addrs = IndexedSeq(
      "3-6 ggg  aaa  ccc ccc main  rrr  eee",
      "9-10 ggg  aaa  ccc ccc main  rrr  eee",
      "10-13 ggg  aaa  ccc ccc main  rrr  eee",
      "14 ggg  aaa  ccc ccc main  rrr  eee"
    )
    val s = AddressSearcher(addrs)
    val r = s search "5-6 ggg aaa  ccc  main  rrr"
    inside(r) {
      case Some(rs) =>
        rs should not be 'multiTops
        rs.top should be (addrs.head)
    }
  }

  it should "deal with crossing number span candidates" in {
    val addrs = IndexedSeq(
      "3-6 ggg  aaa  ccc ccc main  rrr  eee",
      "7-8 ggg  aaa  ccc ccc main  rrr  eee",
      "10-13 ggg  aaa  ccc ccc main  rrr  eee",
      "14 ggg  aaa  ccc ccc main  rrr  eee"
    )
    val s = AddressSearcher(addrs)
    val r = s search "6-7 ggg aaa  ccc  main  rrr"
    inside(r) {
      case Some(rs) =>
        rs should be ('multiTops)
    }
  }
}
