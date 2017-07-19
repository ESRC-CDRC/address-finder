package uk.ac.cdrc.data.utility.text
import scalax.collection.Graph
import scalax.collection.GraphPredef._
/**
  * Created on 7/19/17.
  */
object Utility {
  def condenseAddresses(addressBase: IndexedSeq[(String, Long)]): IndexedSeq[(String, Long)] = {
    if (addressBase.length <= 1)
      addressBase
    else {
      val urnMapping = addressBase.toMap
      val searcher = AddressSearcher(addressBase map {_._1})
      val matched = for {
        (q, urn) <- addressBase
        _r <- searcher search q
        r <- _r.pop
        m <- r.matched
      } yield urn ~ urnMapping(m)
      val matchedGraph = Graph.from(edges = matched ++ (addressBase map {a => a._2 ~ a._2}))
      val reprUrns = (for {
        c <- matchedGraph.componentTraverser()
        nodes = c.nodes.toOuterNodes
        m = nodes.min
        urn <- nodes
      } yield urn -> m).toMap
      for {
        (a, urn) <- addressBase
      } yield a -> reprUrns(urn) // Type mismatch is a false report
    }

  }

}
