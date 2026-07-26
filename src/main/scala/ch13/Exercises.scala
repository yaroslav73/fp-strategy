package ch13

import scala.math.Numeric

@main def foldableExercises01(): Unit =
  // This example is not compile:
  // def foldLeft[B](z: B)(op: (B, A) => B): B
  // so op: (B, A) => B is (elem, acc) => elem :: acc
  // and prepend :: does work for the elem as
  // 1 :: List(2, 3) = List(2, 3).::(1) = List(1, 2, 3)
  // Nil :: 1, should be 1 :: Nil
//  val res01 = List(1, 2, 3).foldLeft(Nil)(_ :: _)

  // compile
  val res02 = List(1, 2, 3).foldRight(Nil)(_ :: _)
  println(res02)

def map[A, B](xs: List[A])(f: A => B): List[B] =
  xs.foldRight(Nil)((elem, acc) => f(elem) :: acc)

def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] =
  xs.foldRight(Nil)((elem, acc) => f(elem) ::: acc)

def filter[A](xs: List[A])(p: A => Boolean): List[A] =
  xs.foldRight(Nil)((elem, acc) => if (p(elem)) elem :: acc else acc)

def sum[A](xs: List[A])(using numeric: Numeric[A]): A =
  xs.foldRight(numeric.zero)(numeric.plus)
