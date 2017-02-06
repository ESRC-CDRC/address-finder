package uk.ac.cdrc.data.utility.text

import org.scalatest.{Matchers, WordSpec}

/**
  * Test analyzers
  */
class AnalyzerSpec extends WordSpec with Matchers{
  "A nested analyzer" should {
    "work" in {
      class NestedWordSeqAnalyzedPool(override val pool: IndexedSeq[String])
        extends PunctuationRemoval[IndexedSeq[String], WordSeqAnalyzer](WordSeqAnalyzer)
          with AnalyzedPool[String, IndexedSeq[String]]
      val nwsap = new NestedWordSeqAnalyzedPool(IndexedSeq("aaa, bbb, ccc", "bbb,ccc,ddd"))
      nwsap.processed.head should contain ("aaa")
    }
  }
}
