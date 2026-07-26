package ch11

import cats.data.EitherT
import cats.syntax.all.*
import ch11.Exercise.{ Response, getPowerLevel }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ExerciseSuite extends munit.FunSuite {
  test("getPowerLevel should return power level if autobot present") {
    assertEquals(getPowerLevel("Jazz"), 6.pure[Response])
  }

  test("getPowerLevel should return error if autobot absent") {
    assertEquals(
      getPowerLevel("Ironhide"),
      EitherT.fromEither[Future](s"Communication error: Ironhide unreachable".asLeft[Int])
    )
  }
}
