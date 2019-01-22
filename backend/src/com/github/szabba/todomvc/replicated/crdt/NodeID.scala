package com.github.szabba.todomvc.replicated.crdt

case class NodeID(rawID: String)

object NodeID {
  implicit val ordering: Ordering[NodeID] =
    Ordering.by(_.rawID)
}
