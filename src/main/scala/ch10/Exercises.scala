package ch10

import cats.{ Eval, MonadError }
import cats.implicits.{ catsSyntaxApplicativeId, catsSyntaxMonadError }

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
