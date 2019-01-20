package com.github.szabba.todomvc.replicated.crdt

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

object NodeIDTestUtil {
  implicit val arbitraryNodeID: Arbitrary[NodeID] = {
    Arbitrary {
      for {
        rawID <- arbitrary[Int]
      } yield {
        NodeID(rawID.toString)
      }
    }
  }
}
