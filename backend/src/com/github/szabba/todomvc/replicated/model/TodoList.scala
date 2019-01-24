package com.github.szabba.todomvc.replicated.model

import com.github.szabba.todomvc.replicated.algebra.JoinSemilattice
import com.github.szabba.todomvc.replicated.crdt.Clock.decoder
import com.github.szabba.todomvc.replicated.crdt.{Clock, NodeID}
import com.github.szabba.todomvc.replicated.model.TodoList.TodoID
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import io.circe.syntax._

import scala.collection.immutable.SortedMap

case class TodoList private (todos: SortedMap[TodoID, Todo]) {

  def add(atNode: NodeID, todo: Todo): (TodoID, TodoList) = {
    val nextNodeID = nextIDAt(atNode)
    val extendedTodos = todos + (nextNodeID -> todo)
    (nextNodeID, TodoList(extendedTodos))
  }

  def update(targetID: TodoID, f: Todo => Todo): TodoList = {
    val updatedTodos =
      todos
        .get(targetID)
        .map(f)
        .map(todo => todos + (targetID -> todo))
        .getOrElse(todos)

    TodoList(updatedTodos)
  }

  def delete(id: TodoID): TodoList =
    TodoList(todos - id)

  def merge(other: TodoList): TodoList = {
    def mergedEntry(id: TodoID): Option[(TodoID, Todo)] =
      (todos.get(id), other.todos.get(id)) match {
        case (Some(left), Some(right)) => Some(id -> left.merge(right))
        case (Some(todo), _)           => Some(id -> todo)
        case (_, Some(todo))           => Some(id -> todo)
        case _                         => None
      }

    val allIDs =
      this.todos.keySet ++ other.todos.keySet

    val mergedTodos =
      SortedMap.empty[TodoID, Todo] ++ allIDs.flatMap(mergedEntry)

    TodoList(mergedTodos)
  }

  private def nextIDAt(atNode: NodeID): TodoID = {
    val currentMax =
      todos.keySet
        .filter(_.atNode == atNode)
        .map(_.no)
        .fold(0)(Math.max)
    TodoID(atNode, currentMax + 1)
  }

}

object TodoList {

  case class TodoID(atNode: NodeID, no: Int)

  object TodoID {
    implicit val ordering: Ordering[TodoID] =
      Ordering.by(id => (id.atNode, id.no))

    implicit val encoder: Encoder[TodoID] =
      Encoder.forProduct2("node", "no") { id =>
        (id.atNode, id.no)
      }

    implicit val decoder: Decoder[TodoID] =
      Decoder.forProduct2("node", "no")(apply)

    implicit val keyEncoder: KeyEncoder[TodoID] =
      KeyEncoder.encodeKeyString.contramap(_.asJson.toString)

    implicit val keyDecoder: KeyDecoder[TodoID] = new KeyDecoder[TodoID] {
      import io.circe.parser._
      override def apply(key: String): Option[TodoID] = {
        parse(key).toOption.flatMap(decoder.decodeJson(_).toOption)
      }
    }
  }

  val empty: TodoList =
    TodoList(SortedMap.empty)

  implicit val encoder: Encoder[TodoList] =
    Encoder.encodeMap[TodoID, Todo].contramap(_.todos)

  implicit val decoder: Decoder[TodoList] =
    Decoder
      .decodeMap[TodoID, Todo]
      .map(_.toIndexedSeq)
      .map(SortedMap.apply(_: _*))
      .map(apply)

  val joinSemilattice: JoinSemilattice[TodoList] = { (left, right) =>
    left.merge(right)
  }

}
