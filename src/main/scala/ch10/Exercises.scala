package ch10

import cats.data.{ Reader, State, Writer }
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

final case class Database(
  usernames: Map[Int, String],
  passwords: Map[String, String]
)

type DatabaseReader[A] = Reader[Database, A]

def findUsername(userId: Int): DatabaseReader[Option[String]] =
  Reader(db => db.usernames.get(userId))

def checkPassword(username: String, password: String): DatabaseReader[Boolean] =
  Reader(db => db.passwords.get(username).contains(password))

def checkLogin(userId: Int, password: String): DatabaseReader[Boolean] =
  for {
    usernameOpt <- findUsername(userId)
    isValid <- usernameOpt match {
      case Some(username) => checkPassword(username, password)
      case None           => Reader[Database, Boolean](_ => false)
    }
  } yield isValid

type CalcState[A] = State[List[Int], A]

def evalOne(sym: String): CalcState[Int] = State[List[Int], Int] { oldStack =>
  def updateStack(stack: List[Int], sym: String): List[Int] = {
    println(s"Evaluating '$sym' with stack: $stack")
    sym match {
      case "+" if stack.size >= 2   => stack.head + stack.tail.head :: stack.tail.tail
      case "*" if stack.size >= 2   => stack.head * stack.tail.head :: stack.tail.tail
      case n if n.forall(_.isDigit) => n.toInt :: stack
    }
  }

  val newStack = updateStack(oldStack, sym)
  val result   = newStack.head

  (newStack, result)
}

def evalAll(input: List[String]): CalcState[Int] =
  input.foldLeft(State.pure[List[Int], Int](0)) { (state, sym) =>
    state.flatMap(_ => evalOne(sym))
  }

def evalInput(input: String): CalcState[Int] =
  evalAll(input.split(" ").toList)
