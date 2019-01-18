package com.github.szabba.todomvc.replicated.algebra

trait JoinSemilattice[A] {
  def merge(left: A, right: A): A
}
