package ch11

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ Await, Future }

object PlainFutureFailed extends App {
  private val powerLevels = Map("Jazz" -> 6, "Bumblebee" -> 8)

  def getPowerLevel(autobot: String): Future[Int] =
    Future(powerLevels.getOrElse(autobot, 0))

  val combined: Future[Int] =
    getPowerLevel("Jazz").flatMap { p1 =>
      getPowerLevel("Bumblebee").map { p2 =>
        p1 + p2
      }
    }
  println("start")
  println(Await.result(combined, 2.seconds)) // deadlocks -> TimeoutException
  println("done")
}
