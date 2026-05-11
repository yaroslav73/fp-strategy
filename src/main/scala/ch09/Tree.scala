package ch09

import cats.Functor

enum Tree[+A] {
  case Branch(left: Tree[A], right: Tree[A]) extends Tree[A]
  case Leaf(value: A) extends Tree[A]
}

object Tree {
  given Functor[Tree] with
    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
      tree match
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
        case Leaf(value)         => Leaf(f(value))
}
