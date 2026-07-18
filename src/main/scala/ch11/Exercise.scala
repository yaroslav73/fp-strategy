package ch11

import cats.data.EitherT
import cats.syntax.all.*

import scala.concurrent.ExecutionContext

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ Await, Future }

object Exercise {
  // Future[Either[String, A]] => EitherT[Future, A, String]
  type Response[A] = EitherT[Future, String, A]

  private lazy val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10,
  )

  def getPowerLevel(autobot: String): Response[Int] =
    EitherT.fromOption(powerLevels.get(autobot), s"Communication error: $autobot unreachable")

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      ally1Power <- getPowerLevel(ally1)
      ally2Power <- getPowerLevel(ally2)
    } yield (ally1Power + ally2Power) > 15

  def tacticalReport(ally1: String, ally2: String): String =
    Await.result(
      canSpecialMove(ally1, ally2).value
        .map {
          case Left(value) => value
          case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
          case Right(false) => s"$ally1 and $ally2 need a recharge."
        },
      5.seconds
    )
}

@main def testTrans(): Unit =
  import Exercise.*
  println(Await.result(canSpecialMove("Jazz", "Bumblebee").value, 2.seconds))
  println(tacticalReport("Jazz", "Bumblebee"))
  println(tacticalReport("Bumblebee", "Hot Rod"))
