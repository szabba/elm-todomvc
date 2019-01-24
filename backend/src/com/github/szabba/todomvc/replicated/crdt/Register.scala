package com.github.szabba.todomvc.replicated.crdt

import com.github.szabba.todomvc.replicated.algebra.JoinSemilattice
import io.circe.{Decoder, Encoder}

case class Register[A] private (private val valuesAt: Map[Clock, Set[A]]) {

  lazy val clock: Clock = {
    valuesAt.keySet.foldLeft(Clock.empty)(Clock.joinSemilattice.merge)
  }

  lazy val values: Set[A] = {
    valuesAt.values.flatten.toSet
  }

  def set(atNode: NodeID, a: A): Register[A] = {
    val tickedClock = clock.advanceAt(atNode)
    val newValuesAt = Map(tickedClock -> Set(a))
    copy(valuesAt = newValuesAt)
  }

  def merge(other: Register[A]): Register[A] = {
    def mergedValues(at: Clock): Set[A] = {
      val local = valuesAt.getOrElse(at, Set.empty)
      val remote = other.valuesAt.getOrElse(at, Set.empty)
      local ++ remote
    }

    val clocks = valuesAt.keySet ++ other.valuesAt.keySet
    val clocksToDrop = for {
      dropCandidate <- clocks
      otherClock <- clocks
      if dropCandidate.comparePartial(otherClock) == PartialOrder.LeftOlder
    } yield dropCandidate
    val clocksToKeep = clocks.diff(clocksToDrop)
    Register(clocksToKeep.map(at => at -> mergedValues(at)).toMap)
  }
}

object Register {

  def empty[A]: Register[A] = {
    Register(Map.empty)
  }

  implicit def encoder[A: Encoder]: Encoder[Register[A]] =
    Encoder.encodeMap[Clock, Set[A]].contramap(_.valuesAt)

  implicit def decoder[A: Decoder]: Decoder[Register[A]] =
    Decoder.decodeMap[Clock, Set[A]].map(apply)

  def joinSemilattice[A]: JoinSemilattice[Register[A]] = { (left, right) =>
    left.merge(right)
  }
}
