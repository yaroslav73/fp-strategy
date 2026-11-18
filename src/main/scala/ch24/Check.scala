package ch24

import cats.data.Validated
import cats.syntax.apply.*
import cats.kernel.Semigroup

// value => value in a context
// type Check[A] => A => Either[String, A]
// error as a String is not the best representation
// type Check[E, A] => A => Either[E, A]

trait Check[E, A] {
  import Check.*

  def apply(a: A)(using s: Semigroup[E]): Validated[E, A] =
    this match {
      case Pure(f)            => f(a)
      case And(first, second) => (first(a), second(a)).mapN((_, _) => a)
    }

  def and(that: Check[E, A]): Check[E, A] = And(this, that)
}

object Check {
  final case class Pure[E, A](f: A => Validated[E, A]) extends Check[E, A]
  final case class And[E, A](first: Check[E, A], second: Check[E, A]) extends Check[E, A]
}
