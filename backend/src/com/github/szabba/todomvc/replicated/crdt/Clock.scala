package com.github.szabba.todomvc.replicated.crdt

import com.github.szabba.todomvc.replicated.algebra.JoinSemilattice

case class Clock private (private val rawTicks: Map[String, Int]) {

  def ticksAt(nodeID: NodeID): Int = {
    rawTicks.getOrElse(nodeID.rawID, 0)
  }

  def advanceAt(nodeID: NodeID): Clock = {
    copy(rawTicks.updated(nodeID.rawID, ticksAt(nodeID) + 1))
  }

  def toMap: Map[NodeID, Int] = {
    rawTicks
      .map(decorateEntryID)
  }

  def comparePartial(clock: Clock): PartialOrder = {
    val rawIDs = clock.rawTicks.keySet ++ rawTicks.keySet
    val tickPairs =
      rawIDs.map(rawID =>
        (ticksAt(NodeID(rawID)), clock.ticksAt(NodeID(rawID))))

    val anyLess = tickPairs.exists { case (left, right) => left < right }
    val anyMore = tickPairs.exists { case (left, right) => left > right }

    (anyLess, anyMore) match {
      case (false, false) => PartialOrder.Equal
      case (true, false)  => PartialOrder.LeftOlder
      case (false, true)  => PartialOrder.RightOlder
      case (true, true)   => PartialOrder.Concurrent
    }
  }

  def merge(other: Clock): Clock = {
    def higherTicks(node: String): (String, Int) = {
      val local = rawTicks.getOrElse(node, 0)
      val remote = other.rawTicks.getOrElse(node, 0)
      (node, Math.max(local, remote))
    }

    val nodes = rawTicks.keySet ++ other.rawTicks.keySet
    val newRawTicks = nodes.map(higherTicks).toMap
    Clock(newRawTicks)
  }

  private def decorateEntryID(rawEntry: (String, Int)): (NodeID, Int) = {
    val (rawID, ticks) = rawEntry
    (NodeID(rawID), ticks)
  }
}

object Clock {
  val empty: Clock = Clock(Map.empty)

  val joinSemilattice: JoinSemilattice[Clock] = { (left, right) =>
    left.merge(right)
  }
}
