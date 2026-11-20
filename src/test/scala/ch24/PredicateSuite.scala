package ch24

import cats.data.Validated
import cats.implicits.catsSyntaxValidatedId
import cats.instances.list.*

class PredicateSuite extends munit.FunSuite {
  val first: Predicate[List[String], Int] = Predicate.Pure { v =>
    if v > 2 then v.valid else List("Must be > 2").invalid
  }

  val second: Predicate[List[String], Int] = Predicate.Pure { v =>
    if v < -2 then v.valid else List("Must be < -2").invalid
  }

  val third: Predicate[List[String], Int] = Predicate.Pure { v =>
    if v < 20 then v.valid else List("Must be < 20").invalid
  }

  test("pure should return valid value") {
    assertEquals(first(3), Validated.Valid(3))
  }

  test("pure should return error") {
    assertEquals(first(2), Validated.Invalid(List("Must be > 2")))
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
  
  test("or should satisfy only one condition (first)") {
    assertEquals(first.or(third)(21), Validated.Valid(21))
  }

  test("or should satisfy only one condition (second)") {
    assertEquals(first.or(second)(-3), Validated.Valid(-3))
  }

  test("or satisfy both conditions") {
    assertEquals(first.or(third)(18), Validated.Valid(18))
  }

  test("or do not satisfy any conditions") {
    assertEquals(first.or(second)(0), Validated.Invalid(List("Must be > 2", "Must be < -2")))
  }

  test("or should not evaluate second if first is valid") {
    val failing: Predicate[List[String], Int] = Predicate.Pure(_ => fail("second must not be evaluated"))
    assertEquals(first.or(failing)(3), Validated.Valid(3))
  }

  test("and inside or should be valid if or branch passes") {
    assertEquals(first.and(third).or(second)(-3), Validated.Valid(-3))
  }

  test("and inside or should collect errors of both branches") {
    assertEquals(
      first.and(third).or(second)(21),
      Validated.Invalid(List("Must be < 20", "Must be < -2")),
    )
  }

  test("or inside and should collect errors of both branches") {
    assertEquals(
      first.or(second).and(third)(0),
      Validated.Invalid(List("Must be > 2", "Must be < -2")),
    )
    assertEquals(first.or(second).and(third)(21), Validated.Invalid(List("Must be < 20")))
  }
}
