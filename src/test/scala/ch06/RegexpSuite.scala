package ch06

class RegexpSuite extends munit.FunSuite {
  test("Regexp: matches") {
    import Regexp.*

    assert(Apply("a").matches("a"))
    assert(!Apply("a").matches("b"))

    assert((Apply("a") ++ Apply("b")).matches("ab"))
    assert(!(Apply("a") ++ Apply("b")).matches("a"))
    assert(!(Apply("a") ++ Apply("b")).matches("b"))
    assert(!(Apply("a") ++ Apply("b")).matches(""))
  }
}
