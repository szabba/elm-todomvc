package com.github.szabba.todomvc.replicated.model

import com.github.szabba.todomvc.replicated.Arbitraries
import com.github.szabba.todomvc.replicated.Arbitraries._
import com.github.szabba.todomvc.replicated.algebra.{JoinSemilattice, JoinSemilatticeTest}
import com.github.szabba.todomvc.replicated.crdt.NodeID
import org.scalacheck.Arbitrary

class TodoListTest extends JoinSemilatticeTest[TodoList] {

  override val joinSemilattice: JoinSemilattice[TodoList] = TodoList.joinSemilattice
  override implicit val arb: Arbitrary[TodoList] = Arbitraries.todoList

  property("an empty list has no todos") {
    assert {
      TodoList.empty.todos.isEmpty
    }
  }

  property("an added todo is included in the list") {
    forAll { (todoList: TodoList, atNode: NodeID, todo: Todo) =>
      val (todoID, extendedTodoList) = todoList.add(atNode, todo)
      assert {
        extendedTodoList.todos.get(todoID).contains(todo)
      }
    }
  }

  property("an added todo's ID is not present in the old todo list") {
    forAll { (todoList: TodoList, atNode: NodeID, todo: Todo) =>
      val (todoID, _) = todoList.add(atNode, todo)
      assert {
        !todoList.todos.contains(todoID)
      }
    }
  }

  property("updated todos change") {
    forAll { (todoList: TodoList, atNode: NodeID, todo: Todo, op: TodoOp) =>
      val (id, withTodo) = todoList.add(atNode, todo)
      val updatedList = withTodo.update(id, op.step)
      withClue("todo with ID ${id} should change") {
        assert {
          updatedList.todos.get(id) != todoList.todos.get(id)
        }
      }
    }
  }

  property("a deleted todo is no longer present") {
    forAll {
      (todoList: TodoList, atNode: NodeID, todo: Todo) =>
        val (id, withTodo) = todoList.add(atNode, todo)
        val withoutTodo = todoList.delete(id)
        assert {
          !withoutTodo.todos.contains(id)
        }
    }
  }

  property("updated todos merge in a merged list") {
    forAll {
      (todoList: TodoList, atNode: NodeID, todo: Todo, leftOp: TodoOp, rightOp: TodoOp) =>
        whenever(leftOp != rightOp) {
          val (id, extendedList) = todoList.add(atNode, todo)
          val leftList = extendedList.update(id, leftOp.step)
          val rightList = extendedList.update(id, rightOp.step)
          val mergedList = leftList.merge(rightList)
          assert {
            mergedList.todos.get(id) == leftList.todos.get(id).flatMap {
              leftTodo => rightList.todos.get(id).map(_.merge(leftTodo))
            }
          }
        }
    }
  }

}
