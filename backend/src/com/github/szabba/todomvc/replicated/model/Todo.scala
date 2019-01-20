package com.github.szabba.todomvc.replicated.model

import com.github.szabba.todomvc.replicated.algebra.JoinSemilattice
import com.github.szabba.todomvc.replicated.crdt.{NodeID, Register}

case class Todo private (private val textRegister: Register[String],
                         isDone: Boolean = false) {

  lazy val text: String =
    textRegister.values.reduceOption(_ ++ " / " ++ _).getOrElse("")

  def setText(atNode: NodeID, newText: String): Todo = {
    val newRegister = textRegister.set(atNode, newText)
    copy(textRegister = newRegister)
  }

  def complete(): Todo =
    copy(isDone = true)

  def merge(other: Todo): Todo =
    Todo(textRegister = textRegister.merge(other.textRegister),
         isDone = isDone || other.isDone)
}

object Todo {

  val empty: Todo =
    Todo(Register.empty)

  val joinSemilattice: JoinSemilattice[Todo] = { (left, right) =>
    left.merge(right)
  }

}
