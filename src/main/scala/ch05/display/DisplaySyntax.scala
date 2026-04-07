package ch05.display

object DisplaySyntax {
  extension [A](value: A)
    def display(using d: Display[A]): String =
      d.display(value)

    def print(using d: Display[A]): Unit =
      println(d.display(value))
}
