package com.github.szabba.todomvc.replicated.crdt

import com.github.szabba.todomvc.replicated.BaseTest
import org.scalacheck.Arbitrary

class ClockTest extends BaseTest {

  import ClockTest._

  property("empty clock converts to an empty map") {
    assert ( Clock.empty.toMap.isEmpty )
  }

  property("never has negative ticks for any node") {
    forAll { clock: Clock =>
      clock.toMap.foreach { case (nodeID, ticks) =>
        assert ( ticks >= 0, nodeID )
      }
    }
  }

  property("is (partial order) equal to itself") {
    forAll { clock: Clock =>
      assert(clock.comparePartial(clock) == PartialOrder.Equal)
    }
  }

  property("clock with an extra tick is the newer of two") {
    forAll { (clock: Clock, extraTick: Tick) =>
      val newerClock = clock.tick(extraTick.atNode)

      assert ( clock.comparePartial(newerClock) == PartialOrder.LeftOlder, (clock, newerClock) )
      assert ( newerClock.comparePartial(clock) == PartialOrder.RightOlder, (newerClock, clock) )
    }
  }

  property("clocks with extra ticks at different nodes are concurrent") {
    forAll { (clock: Clock, leftExtra: Tick, rightExtra: Tick) =>
      whenever(leftExtra != rightExtra) {

        val left = clock.tick(leftExtra.atNode)
        val right = clock.tick(rightExtra.atNode)

        assert ( left.comparePartial(right) == PartialOrder.Concurrent, (left, right) )
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
    clock.tick(tick.atNode)
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