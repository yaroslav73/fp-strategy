package ch24

import cats.data.Validated
import cats.syntax.apply.*
import cats.kernel.Semigroup
import cats.syntax.semigroup.catsSyntaxSemigroup

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
      case Or(first, second) =>
        first(a) match {
          case va @ Validated.Valid(_) => va
          case Validated.Invalid(e1) =>
            second(a) match {
              case va @ Validated.Valid(_) => va
              case Validated.Invalid(e2)   => Validated.Invalid(e1 |+| e2)
            }
        }
    }

  def and(that: Check[E, A]): Check[E, A] = And(this, that)

  def or(that: Check[E, A]): Check[E, A] = Or(this, that)
}

object Check {
  final case class Pure[E, A](f: A => Validated[E, A]) extends Check[E, A]
  final case class And[E, A](first: Check[E, A], second: Check[E, A]) extends Check[E, A]
  final case class Or[E, A](first: Check[E, A], second: Check[E, A]) extends Check[E, A]
}
