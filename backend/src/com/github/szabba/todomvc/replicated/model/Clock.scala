package com.github.szabba.todomvc.replicated.model

case class Clock private (private val rawTicks: Map[String, Int]) {

  def ticks(nodeID: NodeID): Int = {
    rawTicks.getOrElse(nodeID.rawID, 0)
  }

  def tick(nodeID: NodeID): Clock = {
    copy(rawTicks.updated(nodeID.rawID, ticks(nodeID) + 1))
  }

  def toMap: Map[NodeID, Int] = {
    rawTicks
      .map(decorateEntryID)
  }

  def comparePartial(clock: Clock): PartialOrder = {
    val rawIDs = clock.rawTicks.keySet ++ rawTicks.keySet
    val tickPairs =
      rawIDs.map(rawID => (ticks(NodeID(rawID)), clock.ticks(NodeID(rawID))))

    val anyLess = tickPairs.exists { case (left, right) => left < right }
    val anyMore = tickPairs.exists { case (left, right) => left > right }

    (anyLess, anyMore) match {
      case (false, false) => PartialOrder.Equal
      case (true, false)  => PartialOrder.LeftOlder
      case (false, true)  => PartialOrder.RightOlder
      case (true, true)   => PartialOrder.Concurrent
    }
  }

  private def decorateEntryID(rawEntry: (String, Int)): (NodeID, Int) = {
    val (rawID, ticks) = rawEntry
    (NodeID(rawID), ticks)
  }
}

object Clock {
  val empty: Clock = Clock(Map.empty)
}
