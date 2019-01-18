package com.github.szabba.todomvc.replicated.model

sealed trait PartialOrder

object PartialOrder {
  object LeftOlder extends PartialOrder
  object Equal extends PartialOrder
  object RightOlder extends PartialOrder
  object Concurrent extends PartialOrder
}
