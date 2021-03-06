package uk.ac.cdrc.data.utility.text

/**
  * Testing Searcher
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

  "A ngram search" should "find mispelling" in {
    val addr = IndexedSeq(
      "hellosky place summer road seatown seatown",
      "helloworld place summer road seatown seatown"
    )
    val s = NGramBagSearcher(addr)
    val r = s search "hello world summer road seatown"
    inside(r) {
      case Some(rs) => {
        rs.hits should have size 2
        rs.orderedHits(0)._1 should be (1)
        rs.orderedHits(0)._2 should be < rs.orderedHits(1)._2
      }
    }
  }

  "An AddressSearcher" should "deal with single candidate" in {
    val s = AddressSearcher(IndexedSeq("ggg  196  aaa  ccc ccc main  rrr  eee"))
    val r = s search "ggg 20000000000 aaa  ccc  main  rrr"
    inside(r) {
      case Some(rs) =>
        rs.hits should have size 1
        rs.hits.head._2 should be > 100d
        rs.matched shouldBe empty
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
        rs.matched should be (None)
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

  ignore should "deal with number span candidates" in {
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
    val r = s search "6-8 ggg aaa  ccc  main  rrr"
    inside(r) {
      case Some(rs) =>
        rs shouldNot be ('multiTops)
        rs.top should be (addrs(1))
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

  it should "deal with punctuations" in {
    val addrs = IndexedSeq(
      "gggs  aaa  ccc ccc main  rrr  eee",
      "gggs fff aaa  ccc ccc main  rrr  eee",
      "gggs hhh aaa  ccc ccc main  rrr  eee"
    )
    val s = AddressSearcher(addrs)
    val r = s search "ggg's aaa  ccc  main  rrr eee"
    inside(r) {
      case Some(rs) =>
        rs should not be 'multiTops
        rs.top should be (addrs(0))
    }
  }

  it should "find the one with apostrophe" in {
    val addrs = IndexedSeq(
      "gggs  aaa  ccc ccc main  rrr  eee",
      "ggg's aaa  ccc ccc main  rrr  eee",
      "gggs hhh aaa  ccc ccc main  rrr  eee"
    )
    val s = AddressSearcher(addrs)
    val r = s search "ggg's aaa  ccc  main  rrr eee"
    inside(r) {
      case Some(rs) =>
        rs should not be 'multiTops
        rs.top shouldBe addrs(1)
    }
  }

  it should "fail when the only difference is the punctuation" in {
    val addrs = IndexedSeq(
      "gggs aaa  ccc ccc main  rrr  eee",
      "gggs aaa  ccc ccc main  rrr  eee",
      "gggs hhh aaa  ccc ccc main  rrr  eee"
    )
    val s = AddressSearcher(addrs)
    val r = s search "ggg's aaa  ccc  main  rrr eee"
    inside(r) {
      case Some(rs) =>
        rs shouldBe 'multiTops
    }
  }

  ignore should "match numbers from right to left" in {
    val addrs = IndexedSeq(
      "flat 3 2 gggs aaa ccc ccc main rrr eee",
      "flat 2 3 gggs aaa ccc ccc main rrr eee",
      "2 gggs aaa ccc ccc main rrr eee"
    )
    val s = AddressSearcher(addrs)
    val r = s search "3 gggs aaa ccc main  rrr eee"
    inside(r) {
      case Some(rs) =>
        rs shouldNot be ('multiTops)
        rs.orderedHits(0)._1 should be (1)
    }
  }

  it should "match flat alphabet to flat numbers" in {
    val addrs = IndexedSeq(
      "flat 2 2 gggs aaa ccc ccc main rrr eee",
      "flat 3 3 gggs aaa ccc ccc main rrr eee",
      "flat 2 3 gggs aaa ccc ccc main rrr eee",
      "2 gggs aaa ccc ccc main rrr eee"
    )
    val s = AddressSearcher(addrs)
    val r = s search "3c gggs aaa ccc main  rrr eee"
    inside(r) {
      case Some(rs) =>
        rs shouldNot be ('multiTops)
        rs.orderedHits(0)._1 should be (1)
    }
  }

  it should "match alphabet to alphabet flat numbers" in {
    val addrs = IndexedSeq(
      "flat 2 2 gggs aaa ccc ccc main rrr eee",
      "flat 3 3 gggs aaa ccc ccc main rrr eee",
      "3c gggs aaa ccc ccc main rrr eee",
      "flat 2 3 gggs aaa ccc ccc main rrr eee",
      "2 gggs aaa ccc ccc main rrr eee"
    )
    val s = AddressSearcher(addrs)
    val r = s search "3c gggs aaa ccc main  rrr eee"
    inside(r) {
      case Some(rs) =>
        rs shouldNot be ('multiTops)
        rs.orderedHits(0)._1 should be (2)
    }
  }

  it should "match alphabet flat numbers when the region is abbreviated" in {
    val addrs = IndexedSeq(
      "2a aaa bbb eee",
      "2 aaa bbb eec",
      "3a aaa bbb eec"
    )
    val s = AddressSearcher(addrs)
    val r = s search "2a aaa bbb eec"
    inside(r) {
      case Some(rs) =>
        rs shouldNot be ('multiTops)
        rs.orderedHits(0)._1 should be (0)
    }
  }

  it should "find the address with county abbreviation" in {
    val addrs = IndexedSeq(
      "sss ooo kk-dd-mm-qq-cc ppp northumberland",
      "iii uuu kk-dd-mm-qq-cc ppp northumberland",
      "ggg ooo kk-dd-mm-qq-cc ppp northd"
    )
    val s = AddressSearcher(addrs)
    val r = s search "sss ooo kk-dd-mm-qq-cc ppp northd"
    inside(r) {
      case Some(rs) =>
        rs shouldNot be ('multiTops)
        rs.orderedHits(0)._1 should be (0)
    }
  }

  it should "find the address with county names in full and abbreviation" in {
    val addrs = IndexedSeq(
      "10a ooo kk-dd-mm-qq-cc ppp northumberland",
      "10b uuu kk-dd-mm-qq-cc ppp northumberland",
      "10 ooo kk-dd-mm-qq-cc ppp northd"
    )
    val s = AddressSearcher(addrs)
    val r = s search "10a ooo kk-dd-mm-qq-cc ppp northd"
    inside(r) {
      case Some(rs) =>
        rs shouldNot be ('multiTops)
        rs.orderedHits(0)._1 should be (0)
    }
  }

  it should "find the address with abbreviations" in {
    val addrs = IndexedSeq(
      "10a ooo kk-dd-mm-qq-cc ppp street northumberland",
      "10b uuu kk-dd-mm-qq-cc ppp street northumberland",
      "10 ooo kk-dd-mm-qq-cc st ppp northd"
    )
    val s = AddressSearcher(addrs)
    val r = s search "10a ooo kk-dd-mm-qq-cc ppp st northd"
    inside(r) {
      case Some(rs) =>
        rs shouldNot be ('multiTops)
        rs.orderedHits(0)._1 should be (0)
    }
  }

  it should "match to the normalized" in {
    val addrs = IndexedSeq(
      "1st 27 mmm road ooo",
      "27a mmm road ooo",
      "27 mmm rd ddd ooo"
    )
    val s = AddressSearcher(addrs)
    val r = s search "first 27 mmm road ooo"
    inside(r) {
      case Some(rs) =>
        rs shouldNot be ('multiTops)
        rs.orderedHits(0)._1 should be (0)
    }
  }
  it should "match to the normalized 2" in {
    val addrs = IndexedSeq(
      "10 ppp aaa sss ttt www mmm",
      "ground floor flat 10 ppp aaa sss ttt www"
    )
    val s = AddressSearcher(addrs)
    val r = s search "ground 10 ppp aaa sss ttt www mmm"
    inside(r) {
      case Some(rs) =>
        rs shouldNot be ('multiTops)
        rs.orderedHits(0)._1 should be (1)
    }
  }
  it should "match to the normalized 3" in {
    val addrs = IndexedSeq(
      "gr2 15 ccc st nnn ppp mmm",
      "2-2 15 ccc st nnn ppp mmm"
    )
    val s = AddressSearcher(addrs)
    val r = s search "ground 2 15 ccc st nnn ppp mmm"
    inside(r) {
      case Some(rs) =>
        rs shouldNot be ('multiTops)
        rs.orderedHits(0)._1 should be (0)
    }
  }
  it should "match to the normalized 4" in {
    val addrs = IndexedSeq(
      "mmm ddd bbb lane pxc st. kmm buckinghamshire",
      "kkk ddd bbb lane pxc st kmm bucks",
      "fff ppp bbb lane pxc st kmm bucks"
    )
    val s = AddressSearcher(addrs)
    val r = s search "mmm ddd bbb lane pxc st. kmm bucks"
    inside(r) {
      case Some(rs) =>
        rs shouldNot be ('multiTops)
        rs.orderedHits(0)._1 should be (0)
    }
  }
  it should "not matching to wrong number" in {
    val addrs = IndexedSeq(
      "1/0/1 122 some road some city"
    )

    val s = AddressSearcher(addrs)
    val r = s search "0-0-1 122 some road some city lanarkshire"
    inside(r) {
      case Some(rs) =>
        rs.matched should be (None)
    }
  }

  it should "match with apostrophe removed" in {
    val addrs = IndexedSeq(
      "10 aaabbb's road"
    )

    val s = AddressSearcher(addrs)
    val r = s search "10 aaabbbs road"
    inside(r) {
      case Some(rs) =>
        rs.orderedHits(0)._2 should be < 1.0d
    }
  }

  it should "not matching to wrong number 2" in {
    val addrs = IndexedSeq(
      "1a aaabbbs road"
    )

    val s = AddressSearcher(addrs)
    val r = s search "1 aaabbb road"
    inside(r) {
      case Some(rs) =>
        rs.matched should be (None)
    }
  }

  it should "match to the second" in {
    val addrs = IndexedSeq(
      "3 aaabbbs road",
      "1 aaabbb road",
      "1 aaabbbs road",
      "2 aaabbbs road"
    )

    val s = AddressSearcher(addrs)
    val r = s search "1 aaabbb road"
    inside(r) {
      case Some(rs) =>
        inside(rs.pop) {
          case Some(rs2) =>
            rs2.matched should be (Some(addrs(2)))
        }
    }
  }
  it should "match to one" in {
    val addrs = IndexedSeq(
      "10a hww court hww road west kkk mmm",
      "10a hww road mmm",
      "10 hww court hww road west kkk mmm",
      "1a hww court hww road west kkk mmm",
      "1a hww road mmm",
      "1st 2nd floor 12 a hww road mmm",
      "2a hww court hww road west kkk mmm",
      "2a hww road mmm",
      "3a hww court hww road west kkk mmm",
      "3b hww court hww road west kkk mmm",
      "3c hww court hww road west kkk mmm",
      "4a hww court hww road west kkk mmm",
      "4a hww road mmm",
      "4 hww court hww road west kkk mmm",
      "5a hww court hww road west kkk mmm",
      "5 hww court hww road west kkk mmm",
      "5th floor, north 11 hww court hww road mmm",
      "5th floor, sth 12 hww court hww road mmm",
      "6a hww court hww road west kkk mmm",
      "6 hww court hww road west kkk mmm",
      "7a hww road mmm",
      "7 hww court hww road west kkk mmm",
      "8a hww court hww road west kkk mmm",
      "8-a hww road mmm",
      "8 hww court hww road west kkk mmm",
      "9a hww court hww road west kkk mmm",
      "9a hww road mmm",
      "9 hww court hww road west kkk mmm",
      "fifth floor south 12 hww court hww road mmm",
      "fifth nth 11 hww court hww road mmm",
      "flat 1b, hww court hww road mmm",
      "flat 1, hww court hww road mmm",
      "flat 2 hww court hww road west kkk mmm",
      "flat 7a hww court hww road mmm",
      "hww court hww road mmm"
    )

    val s = AddressSearcher(addrs)
    val r = s search "flat 12 hww court hww road mmm"
    inside(r) {
      case Some(rs) =>
        inside(rs.pop) {
          case Some(rs2) =>
            rs2.matched should be (Some(addrs(2)))
        }
    }
  }
}
