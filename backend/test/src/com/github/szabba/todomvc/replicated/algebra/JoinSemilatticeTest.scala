package com.github.szabba.todomvc.replicated.algebra

import com.github.szabba.todomvc.replicated.BaseTest
import org.scalacheck.Arbitrary

trait JoinSemilatticeTest[A] extends BaseTest {

  val joinSemilattice: JoinSemilattice[A]

  import joinSemilattice.merge

  implicit val arb: Arbitrary[A]

  property("merge is associative") {
    forAll { (first: A, second: A, third: A) =>
      assert(merge(first, merge(second, third)) == merge(merge(first, second), third))
    }
  }

  property("merge is commutative") {
    forAll { (first: A, second: A) =>
      assert(merge(first, second) == merge(second, first))
    }
  }

  property("merge is idempotent") {
    forAll { instance: A =>
      assert(merge(instance, instance) == instance)
    }
  }
}
