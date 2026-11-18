package ch24

import cats.instances.list.*
import cats.syntax.all.catsSyntaxEitherId

class CheckSuite extends munit.FunSuite {
  val first: Check[List[String], Int] = Check { v =>
    if v > 2 then v.asRight else List("Must be > 2").asLeft
  }

  val second: Check[List[String], Int] = Check { v =>
    if v < -2 then v.asRight else List("Must be < -2").asLeft
  }

  val third: Check[List[String], Int] = Check { v =>
    if v < 20 then v.asRight else List("Must be < 20").asLeft
  }

  test("and should be valid for both cases") {
    assertEquals(first.and(third)(18), Right(18))
  }

  test("and should collect both errors") {
    assertEquals(first.and(second)(0), Left(List("Must be > 2", "Must be < -2")))
  }

  test("and should return error if only one case passed") {
    assertEquals(first.and(second)(21), Left(List("Must be < -2")))
    assertEquals(first.and(second)(-3), Left(List("Must be > 2")))
  }
}
