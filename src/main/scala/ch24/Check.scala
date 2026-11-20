package ch24

import cats.data.Validated
import cats.kernel.Semigroup

trait Check[E, A, B] {
  import Check.*

  def apply(a: A)(using s: Semigroup[E]): Validated[E, B]

  def map[C](f: B => C): Check[E, A, C] = Map[E, A, B, C](this, f)

  def flatMap[C](f: B => Check[E, A, C]): Check[E, A, C] = FlatMap(this, f)

  def andThen[C](that: Check[E, B, C]): Check[E, A, C] = AndThen(this, that)
}

object Check {
  def apply[E, A](predicate: Predicate[E, A]): Check[E, A, A] = Pure(predicate)

  final case class Pure[E, A](predicate: Predicate[E, A]) extends Check[E, A, A] {
    def apply(a: A)(using s: Semigroup[E]): Validated[E, A] = predicate(a)
  }

  final case class Map[E, A, B, C](check: Check[E, A, B], f: B => C) extends Check[E, A, C] {
    def apply(a: A)(using s: Semigroup[E]): Validated[E, C] =
      check(a).map(f)
  }

  final case class FlatMap[E, A, B, C](check: Check[E, A, B], f: B => Check[E, A, C]) extends Check[E, A, C] {
    def apply(a: A)(using s: Semigroup[E]): Validated[E, C] =
      check(a) match {
        case Validated.Valid(b)        => f(b)(a)
        case ve @ Validated.Invalid(_) => ve
      }
  }

  final case class AndThen[E, A, B, C](first: Check[E, A, B], second: Check[E, B, C]) extends Check[E, A, C] {
    def apply(a: A)(using s: Semigroup[E]): Validated[E, C] = first(a).andThen(b => second(b))
  }
}
