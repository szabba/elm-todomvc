package com.github.szabba.todomvc.replicated

import com.github.szabba.todomvc.replicated.Arbitraries.TodoListOp
import com.github.szabba.todomvc.replicated.Arbitraries.TodoOp.{Complete, SetText}
import com.github.szabba.todomvc.replicated.crdt.{Clock, NodeID, Register}
import com.github.szabba.todomvc.replicated.model.TodoList.TodoID
import com.github.szabba.todomvc.replicated.model.{Todo, TodoList}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}

object Arbitraries {

  implicit val nodeID: Arbitrary[NodeID] =
    Arbitrary {
      arbitrary[Int]
        .map(_.toString)
        .map(NodeID(_))
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

  implicit val todoID: Arbitrary[TodoID] =
    Arbitrary {
      for {
        nodeID <- arbitrary[NodeID]
        anyNo <- arbitrary[Int]
        no = Math.min(1, Math.abs(anyNo))
      } yield {
        TodoID(nodeID, no)
      }
    }

  object TodoListOp {
    case class Add(atNode: NodeID, todo: Todo) extends TodoListOp
    case class Update(todoID: TodoID, op: TodoOp) extends TodoListOp
    case class Delete(todoID: TodoID) extends TodoListOp

    implicit val add: Arbitrary[Add] =
      Arbitrary {
        for {
          atNode <- arbitrary[NodeID]
          todo <- arbitrary[Todo]
        } yield {
          Add(atNode, todo)
        }
      }

    implicit val update: Arbitrary[Update] =
      Arbitrary {
        for {
          todoID <- arbitrary[TodoID]
          op <- arbitrary[TodoOp]
        } yield {
          Update(todoID, op)
        }
      }

    implicit val delete: Arbitrary[Delete] =
      Arbitrary(arbitrary[TodoID].map(Delete))

    implicit val operation: Arbitrary[TodoListOp] =
      Arbitrary(
        Gen.oneOf(
          arbitrary[Add],
          arbitrary[Update],
          arbitrary[Delete]))
  }

  sealed trait TodoListOp {
    def step(todoList: TodoList): TodoList =
      this match {
        case TodoListOp.Add(atNode, todo) => todoList.add(atNode, todo)._2
        case TodoListOp.Update(todoID, op) => todoList.update(todoID, op.step)
        case TodoListOp.Delete(todoID) => todoList.delete(todoID)
      }
  }

  val todoList: Arbitrary[TodoList] = {

    def run(ops: List[TodoListOp]): TodoList =
      ops.foldLeft(TodoList.empty) {
        (todoList, op) => op.step(todoList)
      }

    Arbitrary(arbitrary[List[TodoListOp]].map(run))
  }
}
