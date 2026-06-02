package ch10

import cats.data.Writer
import cats.{ Eval, MonadError }
import cats.implicits.{ catsSyntaxApplicativeId, catsSyntaxMonadError }

import scala.annotation.tailrec

def validateAdult[F[_]](age: Int)(using me: MonadError[F, Throwable]): F[Int] =
  age.pure[F].ensure(new IllegalArgumentException("Age must be 18 or older"))(_ >= 18)

def foldRight[A, B](xs: List[A], acc: B)(f: (A, B) => B): Eval[B] =
  xs match
    case head :: tail => Eval.defer { foldRight(tail, acc)(f).map { b => f(head, b) } }
    case Nil          => Eval.now(acc)

def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] = as match {
  case head :: tail => Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
  case Nil          => acc
}

private def slowly[A](body: => A): A =
  try body
  finally Thread.sleep(100)

def factorial(n: Int): Int = {
  val answer = slowly(if n == 0 then 1 else n * factorial(n - 1))
  println(s"factorial($n) = $answer")
  answer
}

def factorial1(n: Int): Writer[Vector[String], Int] = {
  val thread = Thread.currentThread().getName

  @tailrec
  def loop(acc: Int, writer: Writer[Vector[String], Int]): Writer[Vector[String], Int] =
    if n == 0 || acc > n then writer
    else
      val next = acc + 1
      loop(next, writer.bimap(_ :+ s"[$thread]: factorial($acc) = ${writer.value}", _ * next))

  loop(acc = 1, Writer(Vector(s"[$thread]: Starting factorial($n)"), 1))
}
