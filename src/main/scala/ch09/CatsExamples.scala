package ch09

import cats.*

@main def contravariantInCats(): Unit =
  val showString: Show[String] = Show[String]

  val showSymbol: Show[Symbol] = Contravariant[Show].contramap(showString)(symbol => s"'${symbol.name}'")

  // Alternatively, using the syntax provided by cats:
  import cats.syntax.contravariant.*
  val showSymbol2 = showString.contramap[Symbol](symbol => s"'${symbol.name}'")

  println(showSymbol.show(Symbol("Dave")))

@main def invariantInCats(): Unit =
  import cats.syntax.invariant.* // for imap
  import cats.syntax.semigroup.* // for |+|

  given symbolMonoid: Monoid[Symbol] = Monoid[String].imap(Symbol.apply)(_.name)

  println(Monoid[Symbol].empty)
  println(Symbol("A") |+| Symbol("few") |+| Symbol("words"))

@main def partialUnification(): Unit =
  import cats.syntax.functor.* // for map

  val func1 = (x: Int) => x.toDouble
  val func2 = (x: Double) => x * 2

  val func3 = func1.map(func2)

  println(func3(3))

  val func3a: Int => Double = a => func2(func1(a))
  val func3b: Int => Double = func2.compose(func1)

  import cats.syntax.contravariant.* // for contramap

  // It wouldn't compile without it
  type <=[A, B] = A => B
  type F[A]     = A <= Double

  val func2a: F[Double]     = func2
  val func3c: Int => Double = func2a.contramap(func1)
