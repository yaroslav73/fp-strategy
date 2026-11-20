package ch24

import cats.data.Validated
import cats.syntax.apply.*
import cats.kernel.Semigroup
import cats.syntax.semigroup.catsSyntaxSemigroup

// value => value in a context
// type Check[A] => A => Either[String, A]
// error as a String is not the best representation
// type Check[E, A] => A => Either[E, A]

trait Predicate[E, A] {
  import Predicate.*

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

  def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)

  def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)
}

object Predicate {
  final case class Pure[E, A](f: A => Validated[E, A]) extends Predicate[E, A]
  final case class And[E, A](first: Predicate[E, A], second: Predicate[E, A]) extends Predicate[E, A]
  final case class Or[E, A](first: Predicate[E, A], second: Predicate[E, A]) extends Predicate[E, A]
}
