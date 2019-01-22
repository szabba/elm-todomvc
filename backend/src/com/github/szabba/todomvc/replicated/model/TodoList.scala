package com.github.szabba.todomvc.replicated.model

import com.github.szabba.todomvc.replicated.algebra.JoinSemilattice
import com.github.szabba.todomvc.replicated.crdt.NodeID
import com.github.szabba.todomvc.replicated.model.TodoList.TodoID
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
    implicit val idOrdering: Ordering[TodoID] =
      Ordering.by(id => (id.atNode, id.no))
  }

  val empty: TodoList =
    TodoList(SortedMap.empty)

  val joinSemilattice: JoinSemilattice[TodoList] = { (left, right) =>
    left.merge(right)
  }

}
