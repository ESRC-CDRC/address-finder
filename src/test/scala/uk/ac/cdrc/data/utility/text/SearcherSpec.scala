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
    r should matchPattern {case EmptySearchResult => }
  }

  "An default empty AddressSearcher search" should "return nothing" in {
    val emptyPool = AddressSearcher()
    val r = emptyPool search "a b c"
    r should matchPattern {case EmptySearchResult => }
  }

  "An empty wordbag search" should "return nothing" in {
    val emptyPool = WordBagSearcher(IndexedSeq[String]())
    val r = emptyPool search "a b c"
    r should matchPattern {case EmptySearchResult => }
  }

  "An empty AddressSearcher search" should "return nothing" in {
    val emptyPool = AddressSearcher(IndexedSeq[String]())
    val r = emptyPool search "a b c"
    r should matchPattern {case EmptySearchResult => }
  }

  "A wordbag searcher" should "find multi top matches" in {
    val s = WordBagSearcher(IndexedSeq("a-b c d", "a b c", "c d"))
    val r = s search "a b c"
    inside(r) {
      case rs: SomeSearchResult =>
        rs shouldBe 'multiTops
    }
    inside(r) {
      case rs: SomeSearchResult =>
        rs.hits should have size 3
    }
  }

  "A searcher" should "not throw exception" in {
    val s = WordBagSearcher(IndexedSeq("ggg  196  aaa  ccc ccc main  rrr  eee"))
    val r = s search "ggg 200 aaa  ccc  main  rrr"
    inside(r) {
      case rs: SomeSearchResult =>
        rs.hits should have size 1
    }
  }

  "An AddressSearcher" should "deal with single candidate" in {
    val s = AddressSearcher(IndexedSeq("ggg  196  aaa  ccc ccc main  rrr  eee"))
    val r = s search "ggg 20000000000 aaa  ccc  main  rrr"
    inside(r) {
      case rs: SomeSearchResult =>
        rs.hits should have size 1
        rs.hits(0)._2 should be (100)
    }
  }

  it should "deal with two candidates" in {
    val s = AddressSearcher(IndexedSeq(
      "ggg  196  aaa  ccc ccc main  rrr  eee",
      "ggg  197  aaa  ccc ccc main  rrr  eee"
    ))
    val r = s search "ggg 20000000000 aaa  ccc  main  rrr"
    inside(r) {
      case rs: SomeSearchResult =>
        rs.hits should have size 2
        rs.hits(0)._2 should be (100)
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
      case rs: SomeSearchResult =>
        rs.hits should have size 3
        rs should not be 'multiTops
    }
  }

  it should "deal with difficult suffixed candidates" in {
    val s = AddressSearcher(IndexedSeq(
      "ggg  196a  aaa  ccc ccc main  rrr  eee",
      "ggg  196  aaa  ccc ccc main  rrr  eee",
      "ggg  197  aaa  ccc ccc main  rrr  eee",
      "ggg  197a  aaa  ccc ccc main  rrr  eee"
    ))
    val r = s search "ggg 196 aaa  ccc  main  rrr"
    inside(r) {
      case rs: SomeSearchResult =>
        rs.hits should have size 4
        rs should not be 'multiTops
    }
  }

  it should "deal with hard suffixed candidates" in {
    val s = AddressSearcher(IndexedSeq(
      "flat 4 5 ggg  aaa  ccc ccc main  rrr  eee",
      "flat 5 4 ggg  aaa  ccc ccc main  rrr  eee",
      "flat 6 7 ggg  aaa  ccc ccc main  rrr  eee"
    ))
    val r = s search "Flat 4 5 ggg aaa  ccc  main  rrr"
    inside(r) {
      case rs: SomeSearchResult =>
        rs.hits should have size 3
        rs should not be 'multiTops
    }
  }

  it should "deal with harder suffixed candidates" in {
    val addrs = IndexedSeq(
      "flat a 5 ggg  aaa  ccc ccc main  rrr  eee",
      "flat a 4 ggg  aaa  ccc ccc main  rrr  eee",
      "5 ggg  aaa  ccc ccc main  rrr  eee"
    )
    val s = AddressSearcher(addrs)
    val r = s search "5a ggg aaa  ccc  main  rrr"
    inside(r) {
      case rs: SomeSearchResult =>
        rs.hits should have size 3
        rs should not be 'multiTops
        rs.top should be (addrs.head)
    }
  }

}
