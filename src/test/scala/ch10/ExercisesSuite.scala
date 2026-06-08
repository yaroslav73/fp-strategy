package ch10

import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxFlatMapIdOps, toFlatMapOps}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

class ExercisesSuite extends munit.FunSuite {
  type ExceptionOr[A] = Either[Throwable, A]

  test("validateAdult should return age if 18 or older") {
    val result = validateAdult[ExceptionOr](20)
    assertEquals(result, Right(20))
  }

  test("validateAdult should return an error if under 18") {
    val result = validateAdult[ExceptionOr](16)
    assert(result.isLeft)
    result.left.foreach { error => assertEquals(error.getMessage, "Age must be 18 or older") }
  }

  test("foldRight should correctly fold a list") {
    val result = foldRight(List(1, 2, 3), 0)(_ + _)
    assertEquals(result.value, 6)
  }

  test("foldRight should return the accumulator for an empty list") {
    val result = foldRight(Nil, 42)((_, acc) => acc)
    assertEquals(result.value, 42)
  }

  test("foldRight should be stack safe for large lists") {
    val largeList = (1 to 100000).toList
    val result    = foldRight(largeList, 0)(_ + _)
    assertEquals(result.value, 705082704)
  }

  test("factorial should compute factorial correctly") {
    assertEquals(factorial(5), 120)
  }

  test("factorial should indicate execution") {
    Await.result(
      Future.sequence(Vector(Future(factorial(5)), Future(factorial(5)))),
      5.seconds
    )
  }

  test("factorial1 should indicate execution") {
    val res = Await.result(
      Future.sequence(Vector(Future(factorial1(3)), Future(factorial1(4)))),
      5.seconds
    )

    res.foreach(writer => writer.written.foreach(println))
  }

  test("checkLogin should return true for valid credentials") {
    val db = Database(
      usernames = Map(1 -> "alice"),
      passwords = Map("alice" -> "password123")
    )

    val result = checkLogin(1, "password123").run(db)
    assertEquals(result, true)
  }

  test("checkLogin should return false for invalid credentials") {
    val db = Database(
      usernames = Map(1 -> "alice"),
      passwords = Map("alice" -> "password123")
    )

    val result1 = checkLogin(1, "wrongpassword").run(db)
    assertEquals(result1, false)

    val result2 = checkLogin(2, "password123").run(db)
    assertEquals(result2, false)
  }

  test("evalOne should evaluate simple expressions") {
    assertEquals(evalOne("73").runA(Nil).value, 73)
  }

  test("evalOne combined should evaluate multiple expressions") {
    val program =
      for {
        _      <- evalOne("33")
        _      <- evalOne("40")
        result <- evalOne("+")
      } yield result

    assertEquals(program.runA(Nil).value, 73)
  }

  test("evalAll should evaluate a sequence of expressions") {
    val program = evalAll(List("13", "40", "+", "20", "+"))
    assertEquals(program.runA(Nil).value, 73)
  }

  test("evalAll combined should evaluate a longer sequence of expressions") {
    val program =
      for {
        _      <- evalAll(List("1", "2", "+"))
        _      <- evalAll(List("3", "4", "+"))
        result <- evalOne("*")
      } yield result

    assertEquals(program.runA(Nil).value, 21)
  }

  test("Tree tailRecM should be stack safe") {
    val largeTree = (1 to 100000).foldLeft(Tree.leaf(0)) { (tree, n) =>
      tree.flatMap(x => Tree.leaf(x + n))
    }

    assertEquals(largeTree, Tree.leaf(705082704))
  }
}
