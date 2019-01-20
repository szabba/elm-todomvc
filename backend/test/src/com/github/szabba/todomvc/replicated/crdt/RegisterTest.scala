package com.github.szabba.todomvc.replicated.crdt

import com.github.szabba.todomvc.replicated.Arbitraries
import com.github.szabba.todomvc.replicated.algebra.{JoinSemilattice, JoinSemilatticeTest}
import com.github.szabba.todomvc.replicated.crdt.NodeIDTestUtil.arbitraryNodeID
import org.scalacheck.Arbitrary

class RegisterTest extends JoinSemilatticeTest[Register[Int]] {
  override val joinSemilattice: JoinSemilattice[Register[Int]] = Register.joinSemilattice
  override implicit val arb: Arbitrary[Register[Int]] = Arbitraries.register

  property("an empty register has no values") {
    forAll { nodeID: NodeID =>
      assert {
        Register.empty.values.isEmpty
      }
    }
  }

  property("a register has only the value that has just been set") {
    forAll { (register: Register[Int], atNode: NodeID, newVal: Int) =>
      val updatedRegister = register.set(atNode, newVal)

      assert {
        updatedRegister.values == Set(newVal)
      }
    }
  }

  property("updating a register updates its clock") {
    forAll { (register: Register[Int], atNode: NodeID, newVal: Int) =>
      val updatedRegister = register.set(atNode, newVal)

      assert {
        register.clock.comparePartial(updatedRegister.clock) == PartialOrder.LeftOlder
      }
    }
  }

  property("concurrently updated registers merge to hold two values") {
    forAll { (initial: Register[Int], leftOp: (NodeID, Int), rightOp: (NodeID, Int)) =>
      val (leftAt, leftVal) = leftOp
      val (rightAt, rightVal) = rightOp
      whenever(leftVal != rightVal) {

        val left = initial.set(leftAt, leftVal)
        val right = initial.set(rightAt, rightVal)

        val merged = left.merge(right)

        assert {
          merged.values == Set(leftVal, rightVal)
        }
      }
    }
  }
}