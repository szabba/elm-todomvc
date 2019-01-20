package com.github.szabba.todomvc.replicated.model

import com.github.szabba.todomvc.replicated.Arbitraries
import com.github.szabba.todomvc.replicated.Arbitraries.{nodeID, TodoOp}
import com.github.szabba.todomvc.replicated.Arbitraries.TodoOp.setText
import com.github.szabba.todomvc.replicated.algebra.{JoinSemilattice, JoinSemilatticeTest}
import com.github.szabba.todomvc.replicated.crdt.NodeID
import org.scalacheck.Arbitrary

class TodoTest extends JoinSemilatticeTest[Todo] {
  override val joinSemilattice: JoinSemilattice[Todo] = Todo.joinSemilattice
  override implicit val arb: Arbitrary[Todo] = Arbitraries.todo

  property("an empty todo is not done") {
    assert {
      !Todo.empty.isDone
    }
  }

  property("a completed todo is done") {
    forAll { todo: Todo =>
      val completed = todo.complete()
      assert {
        completed.isDone
      }
    }
  }

  property("an empty todo has no text") {
    assert {
      Todo.empty.text == ""
    }
  }

  property("a todo has the text just set") {
    forAll { (todo: Todo, atNode: NodeID, newText: String) =>
      val updatedTodo = todo.setText(atNode, newText)
      assert {
        updatedTodo.text == newText
      }
    }
  }

  property("merge of concurrently differently restated todos holds merged text") {
    forAll { (initial: Todo, leftOp: TodoOp.SetText, rightOp: TodoOp.SetText) =>
      whenever(leftOp.atNode != rightOp.atNode && leftOp.newText != rightOp.newText) {

        val left = leftOp.step(initial)
        val right = rightOp.step(initial)

        val merged = left.merge(right)

        withClue((merged.text, left.text)) {
          assert { merged.text.contains(left.text) }
        }

        withClue((merged.text, right.text)) {
          assert { merged.text.contains(right.text) }
        }
      }
    }
  }

  property("merge with a done todo is a done todo") {
    forAll { (left: Todo, right: Todo) =>
      whenever(left.isDone || right.isDone) {

        val merged = left.merge(right)
        assert {
          merged.isDone
        }
      }
    }
  }
}
