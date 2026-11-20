package ch24

import cats.data.Validated
import cats.implicits.catsSyntaxValidatedId
import cats.instances.list.*

class CheckSuite extends munit.FunSuite {
  val nonEmpty: Predicate[List[String], String] = Predicate.Pure { s =>
    if s.nonEmpty then s.valid else List("Must be non empty").invalid
  }

  val lowerCase: Predicate[List[String], String] = Predicate.Pure { s =>
    if s == s.toLowerCase then s.valid else List("Must be lower case").invalid
  }

  val atMostThree: Predicate[List[String], Int] = Predicate.Pure { n =>
    if n <= 3 then n.valid else List("Must be <= 3").invalid
  }

  test("pure should return valid value") {
    assertEquals(Check(nonEmpty)("abc"), Validated.Valid("abc"))
  }

  test("pure should return error") {
    assertEquals(Check(nonEmpty)(""), Validated.Invalid(List("Must be non empty")))
  }

  test("pure should collect errors of combined predicate") {
    assertEquals(
      Check(nonEmpty.and(lowerCase))("ABC"),
      Validated.Invalid(List("Must be lower case")),
    )
  }

  test("map should transform valid value") {
    assertEquals(Check(nonEmpty).map(_.length)("abc"), Validated.Valid(3))
  }

  test("map should keep error and not call function") {
    val check = Check(nonEmpty).map[Int](_ => fail("map must not be called"))
    assertEquals(check(""), Validated.Invalid(List("Must be non empty")))
  }

  test("map should compose") {
    assertEquals(Check(nonEmpty).map(_.length).map(_ * 2)("abc"), Validated.Valid(6))
  }

  test("flatMap should return result of second check") {
    val check = Check(nonEmpty).flatMap(_ => Check(lowerCase).map(_.reverse))
    assertEquals(check("abc"), Validated.Valid("cba"))
  }

  test("flatMap should pass original input to second check") {
    val check = Check(nonEmpty)
      .map(_.toUpperCase)
      .flatMap(upper => Check(lowerCase).map(original => (upper, original)))
    assertEquals(check("abc"), Validated.Valid(("ABC", "abc")))
  }

  test("flatMap should return error of first check and not call function") {
    val check = Check(nonEmpty).flatMap[String](_ => fail("flatMap must not be called"))
    assertEquals(check(""), Validated.Invalid(List("Must be non empty")))
  }

  test("flatMap should return error of second check") {
    val check = Check(nonEmpty).flatMap(_ => Check(lowerCase))
    assertEquals(check("ABC"), Validated.Invalid(List("Must be lower case")))
  }

  test("andThen should pass output of first check to second check") {
    val check = Check(nonEmpty).map(_.toLowerCase).andThen(Check(lowerCase))
    assertEquals(check("ABC"), Validated.Valid("abc"))
  }

  test("andThen should change type of checked value") {
    val check = Check(nonEmpty).map(_.length).andThen(Check(atMostThree))
    assertEquals(check("abc"), Validated.Valid(3))
  }

  test("andThen should return error of first check and not evaluate second check") {
    val failing: Predicate[List[String], String] = Predicate.Pure(_ => fail("second must not be evaluated"))
    assertEquals(Check(nonEmpty).andThen(Check(failing))(""), Validated.Invalid(List("Must be non empty")))
  }

  test("andThen should return error of second check") {
    val check = Check(nonEmpty).map(_.length).andThen(Check(atMostThree))
    assertEquals(check("abcd"), Validated.Invalid(List("Must be <= 3")))
  }
}
