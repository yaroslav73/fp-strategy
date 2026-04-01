package ch04

import scala.util.Random
import ch04.Bool.{ False, True }

@main def streamExamples(): Unit =
  val randoms: Stream[Double] =
    Stream.unfold(Random, r => r.nextDouble, identity)

  println("First 5 random numbers (2 times):")
  println(randoms.take(5))
  println(randoms.take(5))
  println()

  lazy val randomsByNeed01: Stream[Double] =
    new Stream[Double] {
      lazy val head: Double         = Random.nextDouble()
      lazy val tail: Stream[Double] = randomsByNeed01
    }

  println("First 5 random numbers by need (2 times):")
  println(randomsByNeed01.take(5))
  println(randomsByNeed01.take(5))
  println()

  val randomsByNeed02: Stream[Double] =
    Stream.unfoldByNeed(Random, r => r.nextDouble, identity)

  println("First 5 random numbers by need (2 times):")
  println(randomsByNeed02.take(5))
  println(randomsByNeed02.take(5))
  println()

@main def boolExamples(): Unit =
  def and(a: Bool, b: Bool): Bool =
    new Bool {
      def iff[A](t: A)(f: A): A =
        a.iff(b)(False).iff(t)(f)
    }

  def or(a: Bool, b: Bool): Bool =
    new Bool {
      def iff[A](t: A)(f: A): A =
        a.iff(True)(b).iff(t)(f)
    }

  def not(a: Bool): Bool =
    new Bool {
      def iff[A](t: A)(f: A): A =
        a.iff(False)(True).iff(t)(f)
    }

  // AND
  println("True  and True:  " + and(True, True).iff("True")("False"))
  println("True  and False: " + and(True, False).iff("True")("False"))
  println("False and True:  " + and(False, True).iff("True")("False"))
  println("False and False: " + and(False, False).iff("True")("False"))
  println()

  // OR
  println("True  or True:  " + or(True, True).iff("True")("False"))
  println("True  or False: " + or(True, False).iff("True")("False"))
  println("False or True:  " + or(False, True).iff("True")("False"))
  println("False or False: " + or(False, False).iff("True")("False"))
  println()

  // NOT
  println("Not True:  " + not(True).iff("True")("False"))
  println("Not False: " + not(False).iff("True")("False"))
  println()

@main def lambdaExamples(): Unit =
  type List[A, B] = (B, (A, B) => B) => B
  // Where B -> head of the list,
  // (A, B) => B -> function to fold the list
  // and => B -> the result of folding the list

  // Church encoding of a linked list using only functions (lambda calculus style).
  // It encodes a list as a fold (catamorphism).
  // A value of type List[A, B] is a function that takes:
  //
  // - B — the base/empty case (analogous to Nil)
  // - (A, B) => B — the combining function (analogous to cons/::)
  // - and returns B — the result of folding the list.
  //
  // This mirrors the signature of foldRight:
  // def foldRight[A, B](nil: B)(cons: (A, B) => B): B
  // So a List[A, B] is its own fold. For example:
  // - The empty list just returns the base case:`(b, f) => b
  // - A list [1, 2, 3] with a sum fold `(0, _ + _) computes 1 + (2 + (3 + 0)) = 6

  val Empty: [A, B] => () => List[A, B] =
    [A, B] => () => (b: B, f: (A, B) => B) => b

  val Pair: [A, B] => (A, List[A, B]) => List[A, B] =
    [A, B] => (head: A, tail: List[A, B]) => (empty: B, f: (A, B) => B) => f(head, tail(empty, f))

  val list: [B] => () => List[Int, B] =
    [B] => () => Pair(1, Pair(2, Pair(3, Empty())))
  println(s"List: $list")
  println(s"List: ${list()("", (b, f) => s"$b :: $f")}")

  val sum = list()(0, (a: Int, b: Int) => a + b)
  println(s"Sum of the list: $sum")

  val product = list()(1, (a: Int, b: Int) => a * b)
  println(s"Product of the list: $product")
  
  
  
