package com.github.szabba.todomvc.replicated

import com.github.szabba.todomvc.replicated.Arbitraries.TodoOp.{Complete, SetText}
import com.github.szabba.todomvc.replicated.crdt.{Clock, NodeID, Register}
import com.github.szabba.todomvc.replicated.model.Todo
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}

object Arbitraries {

  implicit val nodeID: Arbitrary[NodeID] =
    Arbitrary {
      arbitrary[Int]
        .map(_.toString)
        .map(NodeID)
    }

  implicit val clock: Arbitrary[Clock] = {

    def run(locations: List[NodeID]): Clock =
      locations.foldLeft(Clock.empty) {
        (clock, location) => clock.advanceAt(location)
      }

    Arbitrary(arbitrary[List[NodeID]].map(run))
  }

  implicit def register[A: Arbitrary]: Arbitrary[Register[A]] = {

    def run(ops: List[(NodeID, A)]): Register[A] =
      ops.foldLeft(Register.empty[A]) {
        case (register, (at, newVal)) => register.set(at, newVal)
      }

    Arbitrary(arbitrary[List[(NodeID, A)]].map(run))
  }

  object TodoOp {

    case class SetText(atNode: NodeID, newText: String) extends TodoOp

    object Complete extends TodoOp

    implicit val setText: Arbitrary[TodoOp.SetText] =
      Arbitrary {
        for {
          atNode <- arbitrary[NodeID]
          newText <- arbitrary[String]
        } yield {
          SetText(atNode, newText)
        }
      }

    implicit val operation: Arbitrary[TodoOp] =
      Arbitrary(
        Gen.oneOf(
          arbitrary[SetText],
          Gen.const(Complete)))
  }

  sealed trait TodoOp {
    def step(todo: Todo): Todo =
      this match {
        case Complete => todo.complete()
        case SetText(atNode, newText) => todo.setText(atNode, newText)
      }
  }

  implicit val todo: Arbitrary[Todo] = {

    def run(ops: List[TodoOp]): Todo =
      ops.foldLeft(Todo.empty) {
        (todo, op) => op.step(todo)
      }

    Arbitrary(arbitrary[List[TodoOp]].map(run))
  }
}
