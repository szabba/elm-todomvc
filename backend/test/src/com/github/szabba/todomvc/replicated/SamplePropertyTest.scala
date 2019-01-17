package com.github.szabba.todomvc.replicated

import org.scalatest.{Assertions, PropSpec}
import org.scalatest.prop.PropertyChecks


class SamplePropertyTest extends PropSpec with PropertyChecks with Assertions {

  property ("an int equals itself") {
    forAll { x: Int => assert(x == x) }
  }

}
