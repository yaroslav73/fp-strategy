package ch24

import cats.data.Validated
import cats.implicits.catsSyntaxValidatedId
import cats.instances.list.*
import cats.syntax.all.catsSyntaxEitherId

class CheckSuite extends munit.FunSuite {
  val first: Check[List[String], Int] = Check.Pure { v =>
    if v > 2 then v.valid else List("Must be > 2").invalid
  }

  val second: Check[List[String], Int] = Check.Pure { v =>
    if v < -2 then v.valid else List("Must be < -2").invalid
  }

  val third: Check[List[String], Int] = Check.Pure { v =>
    if v < 20 then v.valid else List("Must be < 20").invalid
  }

  test("and should be valid for both cases") {
    assertEquals(first.and(third)(18), Validated.Valid(18))
  }

  test("and should collect both errors") {
    assertEquals(first.and(second)(0), Validated.Invalid(List("Must be > 2", "Must be < -2")))
  }

  test("and should return error if only one case passed") {
    assertEquals(first.and(second)(21), Validated.Invalid(List("Must be < -2")))
    assertEquals(first.and(second)(-3), Validated.Invalid(List("Must be > 2")))
  }
}
