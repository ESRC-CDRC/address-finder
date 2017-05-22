package uk.ac.cdrc.data.utility.text

import org.scalatest.{FlatSpec, Matchers}

/**
  * Test analyzers
  */
class AnalyzerSpec extends FlatSpec with Matchers{
  "A nested analyzer" should "work" in {
      class NestedWordSeqAnalyzedPool(override val pool: IndexedSeq[String])
        extends PunctuationRemoval[IndexedSeq[String], WordSeqAnalyzer](WordSeqAnalyzer)
          with AnalyzedPool[String, IndexedSeq[String]]
      val nwsap = new NestedWordSeqAnalyzedPool(IndexedSeq("aaa, bbb, ccc", "bbb,ccc,ddd"))
      nwsap.processed.head should contain ("aaa")
    }

  "extractNumberSpan" should "work" in new NumberSpanAnalyzer{
    process("aaa bbb ccc 1-4 ccc") should be (Array("1", "2", "3", "4"))
    process("aaa bbb ccc 1-4, 5,6,7 ccc") should be (Array("1", "2", "3", "4", "5", "6", "7"))
    process("aaa bbb ccc 6,7 ccc") should be (Array("6", "7"))
    process("aaa bbb ccc 7 ccc") should be (Array("7"))
    process("aaa bbb ccc ccc") should be (Array())
    process("1-1 28 ddd mmm kkk sss") should be (Array("1", "1", "28"))
    process("1/1 28 ddd mmm kkk sss") should be (Array("1", "1", "28"))
  }

}
