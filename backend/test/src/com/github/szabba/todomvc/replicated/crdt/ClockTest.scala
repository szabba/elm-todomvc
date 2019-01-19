package com.github.szabba.todomvc.replicated.crdt

import com.github.szabba.todomvc.replicated.algebra.{JoinSemilattice, JoinSemilatticeTest}
import org.scalacheck.Arbitrary

class ClockTest extends JoinSemilatticeTest[Clock] {

  override val joinSemilattice: JoinSemilattice[Clock] = Clock.joinSemilattice
  override implicit val arb: Arbitrary[Clock] = ClockTest.arbitraryClock

  import ClockTest.{Tick, arbitraryTick}

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
    forAll { (clock: Clock, extraTick: Tick) =>
      val newerClock = clock.advanceAt(extraTick.atNode)

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
    forAll { (clock: Clock, leftExtra: Tick, rightExtra: Tick) =>
      whenever(leftExtra != rightExtra) {

        val left = clock.advanceAt(leftExtra.atNode)
        val right = clock.advanceAt(rightExtra.atNode)

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

object ClockTest {

  case class Tick(atNode: NodeID)

  def run(tickSequence: List[Tick]): Clock = {
    run(Clock.empty, tickSequence)
  }

  def run(start: Clock, extraTicks: List[Tick]): Clock = {
    extraTicks.foldLeft(start)(step(_, _))
  }

  private def step(clock: Clock, tick: Tick): Clock = {
    clock.advanceAt(tick.atNode)
  }

  implicit lazy val arbitraryClock: Arbitrary[Clock] = {
    val gen = Arbitrary.arbitrary[List[Tick]].map(run)
    Arbitrary(gen)
  }

  implicit lazy val arbitraryTick: Arbitrary[Tick] = {
    val gen = Arbitrary.arbitrary[Int]
      .map(Math.abs)
      .map(_.toString)
      .map(NodeID)
      .map(Tick)
    Arbitrary(gen)
  }
}