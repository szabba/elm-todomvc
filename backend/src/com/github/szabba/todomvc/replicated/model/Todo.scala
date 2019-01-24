package com.github.szabba.todomvc.replicated.model

import com.github.szabba.todomvc.replicated.algebra.JoinSemilattice
import com.github.szabba.todomvc.replicated.crdt.{NodeID, Register}
import io.circe.{Decoder, Encoder}

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

  implicit val encoder: Encoder[Todo] =
    Encoder.forProduct2[Register[String], Boolean, Todo]("text", "isDone")(
      unapply(_).get)

  implicit val decoder: Decoder[Todo] =
    Decoder.forProduct2("text", "isDone")(apply)

  val joinSemilattice: JoinSemilattice[Todo] = { (left, right) =>
    left.merge(right)
  }

}
