package ch06

import Regexp.*

class RegexpSuite extends munit.FunSuite {
  test("Regexp: matches Apply") {
    assert(Apply("a").matches("a"))
    assert(!Apply("a").matches("b"))
  }

  test("Regexp: matches Append") {
    assert((Apply("a") ++ Apply("b")).matches("ab"))
//    assert(!(Apply("a") ++ Apply("b")).matches("a"))
//    assert(!(Apply("a") ++ Apply("b")).matches("b"))
//    assert(!(Apply("a") ++ Apply("b")).matches(""))
  }

  test("Regexp: matches OrElse") {
    assert(Apply("a").orElse(Apply("b")).matches("a"))
    assert(Apply("a").orElse(Apply("b")).matches("b"))
    assert(!Apply("a").orElse(Apply("b")).matches(""))
  }

  test("Regexp: matches Repeat") {
    assert(Apply("a").repeat.matches(""))
    assert(Apply("a").repeat.matches("a"))
    assert(Apply("a").repeat.matches("aa"))
    assert(Apply("a").repeat.matches("aaa"))
  }

  test("Regexp: matches Empty") {
    assert(!Empty.matches(""))
    assert(!Empty.matches("a"))
  }

  test("Regexp: matches OrElse with Repeat") {
    assert(Apply("a").orElse(Apply("b").repeat).matches("a"))
    assert(Apply("a").orElse(Apply("b").repeat).matches(""))
    assert(Apply("a").orElse(Apply("b").repeat).matches("b"))
    assert(Apply("a").orElse(Apply("b").repeat).matches("bb"))
    assert(Apply("a").orElse(Apply("b").repeat).matches("bbb"))
  }

  test("Regexp: fail with StackOverflow") {
    val exception = intercept[StackOverflowError](Regexp("a").repeat.matches("a" * 20000))
    assert(clue(exception.getMessage).contains("StackOverflowError"))
  }
}
