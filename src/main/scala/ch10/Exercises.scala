package ch10

import cats.data.{ Reader, State, Writer }
import cats.{ Eval, Monad, MonadError }
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
  input.foldLeft(State.empty[List[Int], Int]) { (state, sym) =>
    state.flatMap(_ => evalOne(sym))
  }

def evalInput(input: String): CalcState[Int] =
  evalAll(input.split(" ").toList)

enum Tree[+A] {
  case Branch(left: Tree[A], right: Tree[A]) extends Tree[A]
  case Leaf(value: A) extends Tree[A]
}

object Tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)
  def leaf[A](value: A): Tree[A] =
    Leaf(value)

  given monadTree: Monad[Tree] = new Monad[Tree] {
    def pure[A](x: A): Tree[A] = leaf(x)

    def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] =
      fa match {
        case Leaf(value)         => f(value)
        case Branch(left, right) => branch(flatMap(left)(f), flatMap(right)(f))
      }

    def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      def loop(tree: Tree[Either[A, B]]): Tree[B] =
        tree match {
          case Leaf(Left(nextA))   => loop(f(nextA))
          case Leaf(Right(b))      => leaf(b)
          case Branch(left, right) => branch(loop(left), loop(right))
        }

      loop(f(a))
    }

    // TODO: fix the implementation
    def tailRecM1[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      type C = B => Call

      enum Call {
        case Loop(expr: Tree[Either[A, B]], c: C)
        case Continue(value: B, c: C)
        case Done(value: B)
      }

      def loop(tree: Tree[Either[A, B]], c: C): Call =
        tree match {
          case Leaf(Left(nextA))   => Call.Loop(f(nextA), c)
          case Leaf(Right(b))      => Call.Continue(b, c)
          case Branch(left, right) => Call.Loop(left, b => loop(right, c))
        }

      @tailrec
      def trampoline(next: Call): B =
        next match {
          case Call.Loop(expr, c)      => trampoline(loop(expr, c))
          case Call.Continue(value, c) => trampoline(c(value))
          case Call.Done(value)        => value
        }

      leaf(trampoline(loop(f(a), b => Call.Done(b))))
    }
  }

  def tailRecM2[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {

    @tailrec
    def loop(open: List[Tree[Either[A, B]]], closed: List[Option[Tree[B]]]): List[Tree[B]] =
      open match {
        case Leaf(Left(nextA)) :: rest   => loop(f(nextA) :: rest, closed)
        case Leaf(Right(b)) :: rest      => loop(rest, Some(leaf(b)) :: closed)
        case Branch(left, right) :: rest => loop(left :: right :: rest, None :: closed)
        case Nil =>
          closed.foldLeft(Nil: List[Tree[B]]) { (acc, optTree) =>
            optTree.map(_ :: acc).getOrElse {
              acc match {
                case left :: right :: tail => branch(left, right) :: tail
                case t                     => t
              }
            }
          }
      }

    loop(List(f(a)), Nil).head
  }
}
