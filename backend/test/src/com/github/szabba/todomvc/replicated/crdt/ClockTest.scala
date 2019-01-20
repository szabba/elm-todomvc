package com.github.szabba.todomvc.replicated.crdt

import com.github.szabba.todomvc.replicated.Arbitraries
import com.github.szabba.todomvc.replicated.Arbitraries.nodeID
import com.github.szabba.todomvc.replicated.algebra.{JoinSemilattice, JoinSemilatticeTest}
import org.scalacheck.Arbitrary

class ClockTest extends JoinSemilatticeTest[Clock] {

  override val joinSemilattice: JoinSemilattice[Clock] = Clock.joinSemilattice
  override implicit val arb: Arbitrary[Clock] = Arbitraries.clock

  property("empty clock converts to an empty map") {
    assert {
      Clock.empty.toMap.isEmpty
    }
  }

  property("never has negative ticks for any node") {
    forAll { clock: Clock =>

      clock.toMap.foreach { case (node, ticks) =>
        withClue("at ${node}:") {
          assert {
            ticks >= 0
          }
        }
      }
    }
  }

  property("is (partial order) equal to itself") {
    forAll { clock: Clock =>
      assert {
        clock.comparePartial(clock) == PartialOrder.Equal
      }
    }
  }

  property("clock with an extra tick is the newer of two") {
    forAll { (clock: Clock, extraTickAt: NodeID) =>
      val newerClock = clock.advanceAt(extraTickAt)

      withClue("comparing ${clock} to {$newClock}:") {
        assert {
          clock.comparePartial(newerClock) == PartialOrder.LeftOlder
        }
      }

      withClue("comparing ${newClock} to ${clock}:") {
        assert {
          newerClock.comparePartial(clock) == PartialOrder.RightOlder
        }
      }
    }
  }

  property("clocks with extra ticks at different nodes are concurrent") {
    forAll { (clock: Clock, leftExtraAt: NodeID, rightExtraAt: NodeID) =>
      whenever(leftExtraAt != rightExtraAt) {

        val left = clock.advanceAt(leftExtraAt)
        val right = clock.advanceAt(rightExtraAt)

        withClue(s"the clocks are ${left} and ${right}:") {
          assert {
            left.comparePartial(right) == PartialOrder.Concurrent
          }
        }
      }
    }
  }

  property("chooses the higher tick count per node from two merged clocks") {
    forAll { (first: Clock, second: Clock) =>
      whenever(first.comparePartial(second) == PartialOrder.Concurrent) {

        val merged = first.merge(second)
        val nodes = first.toMap.keySet ++ second.toMap.keySet

        nodes.foreach { node =>
          withClue("at ${node}:") {
            assert {
              Math.max(first.ticksAt(node), second.ticksAt(node)) == merged.ticksAt(node)
            }
          }
        }
      }
    }
  }
}
