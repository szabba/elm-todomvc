package com.github.szabba.todomvc.replicated

import com.github.szabba.todomvc.replicated.crdt.{Clock, NodeID, Register}
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

object Arbitraries {

  implicit val nodeID: Arbitrary[NodeID] =
    Arbitrary {
      for {
        rawID <- arbitrary[Int]
      } yield {
        NodeID(rawID.toString)
      }
    }

  implicit val clock: Arbitrary[Clock] =
    Arbitrary {
      for {
        nodeIDs <- arbitrary[List[NodeID]]
      } yield {
        nodeIDs.foldLeft(Clock.empty) {
          (clock, nodeID) => clock.advanceAt(nodeID)
        }
      }
    }

  implicit def register[A: Arbitrary]: Arbitrary[Register[A]] =
    Arbitrary {
      for {
        ops <- arbitrary[List[(NodeID, A)]]
      } yield {
        ops.foldLeft(Register.empty[A]) {
          case (register, (at, newVal)) => register.set(at, newVal)
        }
      }
    }
}
