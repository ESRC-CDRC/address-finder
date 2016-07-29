package uk.ac.cdrc.data.utility.text

/**
  * Created  on 7/20/16.
  */


import org.scalatest._

class TokenizerSpec extends FlatSpec with Matchers{

  "An tokenizer" should "work" in {
    SimpleTokenizer tokenize "a b c" should be (Array("a", "b", "c"))
    SimpleTokenizer tokenize "11a b c" should be (Array("11", "a", "b", "c"))
    SimpleTokenizer tokenize "  11a b c11  " should be (Array("11", "11", "a", "b", "c"))
    SimpleTokenizer tokenize "  11-12a b c11  " should be (Array("11", "12", "11", "a", "b", "c"))
    SimpleTokenizer tokenize "  11c-12a b c11  " should be (Array("11", "12", "11", "c", "a", "b", "c"))
  }

}
