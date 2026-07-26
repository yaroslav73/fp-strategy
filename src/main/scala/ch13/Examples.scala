package ch13

import cats.Foldable
import cats.data.Validated

@main def foldableCatsExamples(): Unit =
  Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _)
  Foldable[Option].foldLeft(Option(10), 10)(_ * _)

  // foldLeft defines in terms of Eval for safety

  println(Foldable[List].combineAll(List(1, 2, 3)))
  println(Foldable[List].foldMap(List(1, 2, 3))(_.toString))

  val ints = List(Vector(1, 2, 3), Vector(4, 5, 6))
  println(Foldable[List].compose(using Foldable[Vector]).combineAll(ints))

@main def traverseFutureExamples(): Unit =
  import scala.concurrent.*
  import scala.concurrent.duration.*
  import scala.concurrent.ExecutionContext.Implicits.global

  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com",
  )

  def getUptime(hostname: String): Future[Int] =
    Future(hostname.length * 60)

  val allUptimes01: Future[List[Int]] =
    hostnames.foldLeft(Future(List.empty[Int])) { (acc, host) =>
      val uptime = getUptime(host)
      for {
        acc    <- acc
        uptime <- uptime
      } yield acc :+ uptime
    }

  println(Await.result(allUptimes01, 1.second))

  val allUptimes02: Future[List[Int]] =
    Future.traverse(hostnames)(getUptime)

  println(Await.result(allUptimes02, 1.second))

  // Traversing  with Applicatives

  import cats.syntax.all.*

  def oldCombine(accum: Future[List[Int]], host: String): Future[List[Int]] = {
    val uptime = getUptime(host)
    for {
      accum  <- accum
      uptime <- uptime
    } yield accum :+ uptime
  }

  def newCombine(accum: Future[List[Int]], host: String): Future[List[Int]] =
    (accum, getUptime(host)).mapN(_ :+ _)

  import cats.Applicative

  def listTraverse[F[_]: Applicative, A, B](xs: List[A])(f: A => F[B]): F[List[B]] =
    xs.foldLeft(List.empty[B].pure[F]) { (acc, elem) => (acc, f(elem)).mapN(_ :+ _) }

  def listSequence[F[_]: Applicative, A](xs: List[F[A]]): F[List[A]] =
    listTraverse(xs)(identity)

  val totalUptime = listTraverse(hostnames)(getUptime)

  println(Await.result(totalUptime, 1.second))

  println(listSequence(List(Vector(1, 2), Vector(3, 4))))
  // Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))

  println(listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))))
  // Vector(List(1, 3, 5), List(1, 3, 6), List(1, 4, 5), List(1, 4, 6), List(2, 3, 5), List(2, 3, 6), List(2, 4, 5), List(2, 4, 6))

  println(List(Vector(1, 2), Vector(3, 4)).foldLeft(Vector(List.empty[Int])) { (acc, elem) =>
    println(s"acc: $acc, elem: $elem")
    (acc, elem).mapN { (a, e) =>
      println(s"a: $a, e: $e")
      a :+ e
    }
  })

  def process(inputs: List[Int]): Option[List[Int]] =
    listTraverse(inputs)(n => if n % 2 == 0 then Some(n) else None)

  println(process(List(1, 2, 3)))
  println(process(List(2, 4, 6)))

  type ErrorsOr[A] = Validated[List[String], A]

  def validate(inputs: List[Int]): ErrorsOr[List[Int]] =
    listTraverse(inputs) { n =>
      if n % 2 == 0 then Validated.valid(n)
      else Validated.invalid(List(s"$n is not event"))
    }

  println(validate(List(1, 2, 3)))
  println(validate(List(2, 4, 6)))
