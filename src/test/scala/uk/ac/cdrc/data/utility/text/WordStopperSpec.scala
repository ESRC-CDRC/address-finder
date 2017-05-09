package uk.ac.cdrc.data.utility.text

/**
  * Testing WordStopper
  */
import org.scalatest._

class WordStopperSpec extends FlatSpec with Matchers{

  "An tokenizer" should "work" in new WordStopper {
    filter("flat aaa bbb") should be (" aaa bbb")
    filter("aaa bbb") should be ("aaa bbb")
    filter("flat flat aaa bbb") should be ("  aaa bbb")
    filter("flat flat") should be (" ")
  }

}

