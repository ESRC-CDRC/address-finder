package uk.ac.cdrc.data.utility.text

/**
  * Created  on 7/20/16.
  */


import org.scalatest._

class SearcherTestSpec extends FlatSpec {
  val s = new WordBagSearcher(Array("a b c d", "a b c", "c d"))
  "A wordbag searcher" should "find multi top matches" in {
    assert((s search "a b c").multiTops)
  }
}
