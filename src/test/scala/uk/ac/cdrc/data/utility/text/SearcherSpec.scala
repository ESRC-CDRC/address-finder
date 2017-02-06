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
        rs.getMatching shouldBe empty
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

  it should "deal with crossing number span candidates 2" in {
    val addrs = IndexedSeq(
      "8a www bbb lll iii of uuu",
      "6d www bbb lll iii of uuu",
      "3 www bbb lll iii of uuu",
      "10a www bbb lll iii of uuu",
      "11a www bbb lll iii of uuu",
      "12a www bbb lll iii of uuu",
      "g/l 13 www bbb lll iii of uuu",
      "14a www bbb lll iii of uuu",
      "g/l 15 www bbb lll iii of uuu",
      "16a www bbb lll iii of uuu",
      "17a www bbb lll iii of uuu",
      "18a www bbb lll iii of uuu",
      "19a www bbb lll iii of uuu",
      "4a www bbb lll iii of uuu",
      "5a www bbb lll iii of uuu",
      "6a www bbb lll iii of uuu",
      "7a www bbb lll iii of uuu",
      "9a www bbb lll iii of uuu",
      "1b www bbb lll iii of uuu",
      "1c www bbb lll iii of uuu",
      "1d www bbb lll iii of uuu",
      "1e www bbb lll iii of uuu",
      "mmm ooo www bbb lll iii of uuu",
      "lll ttt ltd 1 www bbb lll iii of uuu",
      "g/r 13 www bbb lll iii of uuu",
      "1/l 13 www bbb lll iii of uuu",
      "1/r 13 www bbb lll iii of uuu",
      "g/r 15 www bbb lll iii of uuu",
      "1/l 15 www bbb lll iii of uuu",
      "1/r 15 www bbb lll iii of uuu",
      "4b www bbb lll iii of uuu",
      "4c www bbb lll iii of uuu",
      "4d www bbb lll iii of uuu",
      "4e www bbb lll iii of uuu",
      "4f www bbb lll iii of uuu",
      "5b www bbb lll iii of uuu",
      "5c www bbb lll iii of uuu",
      "5d www bbb lll iii of uuu",
      "5e www bbb lll iii of uuu",
      "5f www bbb lll iii of uuu",
      "6b www bbb lll iii of uuu",
      "6c www bbb lll iii of uuu",
      "6e www bbb lll iii of uuu",
      "6f www bbb lll iii of uuu",
      "9b www bbb lll iii of uuu",
      "17b www bbb lll iii of uuu",
      "18b www bbb lll iii of uuu",
      "10b www bbb lll iii of uuu",
      "11b www bbb lll iii of uuu",
      "12b www bbb lll iii of uuu",
      "14b www bbb lll iii of uuu",
      "16b www bbb lll iii of uuu",
      "19b www bbb lll iii of uuu",
      "7b www bbb lll iii of uuu",
      "8b www bbb lll iii of uuu",
      "0/2 13 www bbb lll iii of uuu",
      "0/2 15 www bbb lll iii of uuu",
      "1/1 13 www bbb lll iii of uuu",
      "1/1 15 www bbb lll iii of uuu",
      "1/2 13 www bbb lll iii of uuu",
      "1/2 15 www bbb lll iii of uuu"
    )
    val s = AddressSearcher(addrs)
    val r = s search "8a www bbb lll  iii of uuu iii of uuu"
    inside(r) {
      case Some(rs) =>
        rs should not be 'multiTops
        rs.top should be (addrs.head)
    }
  }
}
