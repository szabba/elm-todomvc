package com.github.szabba.todomvc.replicated.crdt

import com.github.szabba.todomvc.replicated.algebra.JoinSemilattice
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import io.circe.syntax._

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

  def fromMap(raw: Map[NodeID, Int]): Clock = {
    val ticks =
      raw.filter(_._2 >= 0).map { case (id, v) => (id.rawID, v) }
    Clock(ticks)
  }

  implicit val encoder: Encoder[Clock] =
    Encoder.encodeMap[NodeID, Int].contramap(_.toMap)

  implicit val decoder: Decoder[Clock] =
    Decoder.decodeMap[NodeID, Int].map(fromMap)

  implicit val keyEncoder: KeyEncoder[Clock] =
    KeyEncoder.encodeKeyString.contramap(_.asJson.toString)

  implicit val keyDecoder: KeyDecoder[Clock] = new KeyDecoder[Clock] {
    import io.circe.parser._
    override def apply(key: String): Option[Clock] = {
      parse(key).toOption.flatMap(decoder.decodeJson(_).toOption)
    }
  }

  val joinSemilattice: JoinSemilattice[Clock] = { (left, right) =>
    left.merge(right)
  }
}
