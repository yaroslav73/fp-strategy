package ch09

class ContramapSuite extends munit.FunSuite {
  given stringDisplay: Display[String] with {
    def display(value: String): String =
      s"'$value'"
  }

  given booleanDisplay: Display[Boolean] with {
    def display(value: Boolean): String =
      if value then "yes" else "no"
  }

  test("Display: contramap with String") {
    assertEquals(Display("Hello"), "'Hello'")
  }

  test("Display: contramap with Boolean") {
    assertEquals(Display(true), "yes")
    assertEquals(Display(false), "no")
  }

  test("Display: contramap with custom type") {
    final case class Box[A](value: A)
    object Box {
      // common generic implementation
//      given [A](using d: Display[A]): Display[Box[A]] with {
//        def display(box: Box[A]): String = d.display(box.value)
//      }

      // Using contramap
      given [A](using d: Display[A]): Display[Box[A]] =
        d.contramap(box => box.value)
    }

    assertEquals(Display(Box("Hello")), "'Hello'")
    assertEquals(Display(Box(true)), "yes")
  }
}
