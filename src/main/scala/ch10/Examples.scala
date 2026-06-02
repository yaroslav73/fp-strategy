package ch10

import cats.data.{ Reader, Writer }
import cats.{ Eval, Monad, MonadError }
import cats.syntax.all.*

import scala.util.Try

// syntax
@main def monadSyntaxExamples(): Unit =
  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    for {
      y <- a
      x <- b
    } yield x * x + y * y

  println(s"Options sum square: ${sumSquare(Option(3), Option(4))}")
  println(s"Options sum square: ${sumSquare(None, Option(4))}")
  println(s"Lists sum square: ${sumSquare(List(1, 2), List(3, 4))}")

@main def eitherMonadExamples(): Unit = {
  val r1 = Right(10).flatMap(x => Right(x * 2))
  val r2 = Right(10).flatMap(x => Left("Oh, no!"))
  val r3 = for {
    a <- Right(1)
    b <- Right(0)
    c <- if b == 0 then Left("Division by zero") else Right(a / b)
  } yield c

  type Result[A] = Either[Throwable, A] // Throwable is the supertype of all exceptions, too general

  enum LoginError:
    case UserNotFound(username: String)
    case InvalidPassword(username: String)
    case UnexpectedError(message: String)

  final case class User(username: String, password: String)

  type LoginResult[A] = Either[LoginError, User]

  def handleError(error: LoginError): Unit =
    error match
      case LoginError.UserNotFound(username)    => println(s"User '$username' not found.")
      case LoginError.InvalidPassword(username) => println(s"Invalid password for user '$username'.")
      case LoginError.UnexpectedError(message)  => println(s"An unexpected error occurred: $message")

  val r4 = Right(User("dave", "passw0rd"))
  val r5 = Left(LoginError.UserNotFound("dave"))

  r4.fold(handleError, println)
  r5.fold(handleError, println)
}

@main def eitherMonadCatsSyntaxExamples(): Unit = {
  // This code will not compile
//  def countPositiveFailed(nums: List[Int]) =
//    nums.foldLeft(Right(0)) { (acc, n) =>
//      if n > 0 then acc.map(_ + 1)
//      else Left(s"Negative number found: $n")
//    }

  // This code will compile, we need to specify the type of the initial value for foldLeft
  def countPositiveEither(nums: List[Int]): Either[String, Int] =
    nums.foldLeft[Either[String, Int]](Right(0)) { (acc, n) =>
      if n > 0 then acc.map(_ + 1)
      else Left(s"Negative number found: $n")
    }

  def countPositive(nums: List[Int]): Either[String, Int] =
    nums.foldLeft(0.asRight[String]) { (acc, n) =>
      if n > 0 then acc.map(_ + 1)
      else s"Negative number found: $n".asLeft[Int]
    }

  println(Either.catchOnly[NumberFormatException]("abc".toInt))

  println(Either.catchNonFatal("abc".toInt))
  println(Either.catchNonFatal(sys.error("crap")))

  println(Either.fromTry(Try("abc".toInt)))
  println(Either.fromOption(None, "No value found"))

  val r6 = "error".asLeft[Int].recover { case _: String => -1 }
  println(r6)

  val r7 = "error".asLeft[Int].recoverWith { case _: String => Right(-1) }
  println(r7)

  println("error".asLeft[Int].leftMap(_.reverse))
  println(7.asRight[String].bimap(_.reverse, _ * 3))
  println("error".asLeft[Int].bimap(_.reverse, _ * 3))
}

@main def monadErrorExamples(): Unit = {
  type ErrorOr[A] = Either[String, A]

  val monadError = MonadError[ErrorOr, String]

  val success = monadError.pure(42)
  val failure = monadError.raiseError("Something went wrong")

  val r1 = monadError.handleErrorWith(failure) {
    case "Something went wrong" => monadError.pure("Expected error")
    case other                  => monadError.raiseError("It's not the error we expected: " + other)
  }
  println(r1)

  val r2 = monadError.handleError(failure) {
    case "Something went wrong" => 73
    case _                      => -1
  }
  println(r2)

  val r3 = monadError.ensure(success)("Value must be positive")(_ > 0)
  println(r3)

  val r4 = monadError.ensure(success)("Value must be greater than 100")(_ > 100)
  println(r4)
}

@main def evalMonadExamples(): Unit = {
  val x = {
    println("Evaluating x")
    math.random()
  }

  println(s"x: $x")
  println(s"x: $x")
  println()

  def y = {
    println("Evaluating y")
    math.random()
  }

  println(s"y: $y")
  println(s"y: $y")
  println()

  lazy val z = {
    println("Evaluating z")
    math.random()
  }

  println(s"z: $z")
  println(s"z: $z")
  println()

  val a = Eval.now {
    println("Evaluating a")
    math.random()
  }

  println(s"a: ${a.value}")
  println(s"a: ${a.value}")
  println()

  val b = Eval.always {
    println("Evaluating b")
    math.random()
  }

  println(s"b: ${b.value}")
  println(s"b: ${b.value}")
  println()

  val c = Eval.later {
    println("Evaluating c")
    math.random()
  }

  println(s"c: ${c.value}")
  println(s"c: ${c.value}")
  println()

  // Scala      Cats      Properties
  // val        Now       Evaluated immediately (eager), memoized
  // def        Always    Evaluated every time (lazy), not memoized
  // lazy val   Later     Evaluated once when first accessed (lazy), memoized

  val greeting = Eval
    .always { println("Step 1"); "Hello" }
    .map { s =>
      println("Step 2"); s"$s, World!"
    }

  greeting.value

  val answer =
    for {
      a <- Eval.now { println("Calculating a"); 40 }
      b <- Eval.now { println("Calculating b"); 33 }
    } yield {
      println("Calculating the answer")
      a + b
    }

  answer.value
  answer.value

  val saying = Eval
    .always { println("Step 1"); "The cat" }
    .map { s =>
      println("Step 2"); s"$s sat on "
    }
    .memoize
    .map { s =>
      println("Step 3"); s"$s the mat."
    }

  saying.value
  saying.value

  def factorialSimple(n: Int): BigInt =
    if n == 1 then n else n * factorialSimple(n - 1)

//  factorialSimple(50000) // StackOverflowError

  def factorialEval(n: Int): Eval[BigInt] =
    if n == 1 then Eval.now(n) else factorialEval(n - 1).map(_ * n)

//  factorialEval(50000).value // StackOverflowError

  def factorial(n: BigInt): Eval[BigInt] =
    if n == 1 then Eval.now(n) else Eval.defer(factorial(n - 1).map(_ * n))

  factorial(50000).value // Works fine
}

@main def writerMonadExamples(): Unit = {
  // type Writer[W, A] = WriterT[Id, W, A]
  val res01 = Writer(Vector("It was the best of times", "It was the worst of times"), 1859)

  type Logged[A] = Writer[Vector[String], A]

  val res02 = 123.pure[Logged]
  println(res02)

  val res03 = Vector("Message 1", "Message 2").tell
  println(res03)

  val res04 = Writer(Vector("Message 1", "Message 2"), 123)
  println(res04)

  val res05 = 123.writer(Vector("Message 1", "Message 2"))
  println(res05)

  val aResult = res05.value
  val aLog    = res05.written
  println(s"aResult: $aResult and aLog: $aLog")

  val (log, result) = res05.run
  println(s"result: $result and log: $log")

  val writer01 = for {
    a <- 10.pure[Logged]
    _ <- Vector("Added 10").tell
    b <- 63.writer(Vector("Added 63"))
  } yield a + b
  println(s"writer01: ${writer01.run}")

  val writer02 = writer01.mapWritten(_.map(_.toUpperCase))
  println(s"writer02: ${writer02.run}")

  val writer03 = writer01.bimap(log => log.map(_.toUpperCase), result => result * 2)
  println(s"writer03: ${writer03.run}")

  val writer04 = writer01.mapBoth((log, result) => (log.map(_.toUpperCase), result * 2))
  println(s"writer04: ${writer04.run}")

  val writer05 = writer01.reset
  println(s"writer05: ${writer05.run}")

  val writer06 = writer01.swap
  println(s"writer06: ${writer06.run}")
}

@main def readerMonadExamples(): Unit = {
  final case class Cat(name: String, favoriteFood: String)

  val catName: Reader[Cat, String] = Reader(cat => cat.name)

  println(catName.run(Cat("Garfield", "Lasagna")))

  val greetKitty: Reader[Cat, String] = catName.map(name => s"Hello, $name!")
  println(greetKitty.run(Cat("Garfield", "Lasagna")))

  val feedKitty: Reader[Cat, String] = Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}!")

  val greetAndFeed: Reader[Cat, String] =
    for {
      greet <- greetKitty
      feed  <- feedKitty
    } yield s"$greet $feed"

  println(greetAndFeed(Cat("Garfield", "Lasagna")))

  println(greetAndFeed(Cat("Heathcliff", "Fish Sticks")))
}
