package ch24

import cats.implicits.{ catsSyntaxEitherId, catsSyntaxSemigroup }
import cats.kernel.Semigroup

// value => value in a context
// type Check[A] => A => Either[String, A]
// error as a String is not the best representation
// type Check[E, A] => A => Either[E, A]

final case class Check[E, A](f: A => Either[E, A]) {
  def apply(value: A): Either[E, A] = f(value)

  def and(that: Check[E, A])(using Semigroup[E]): Check[E, A] = Check { a =>
    (this(a), that(a)) match {
      case (Left(e1), Left(e2)) => (e1.combine(e2)).asLeft
      case (Left(e), Right(_))  => e.asLeft
      case (Right(_), Left(e))  => e.asLeft
      case (Right(_), Right(_)) => a.asRight
    }
  }
}
