package ch09

trait Display[A] { self =>
  def display(value: A): String

  def contramap[B](f: B => A): Display[B] = new Display[B] {
    def display(value: B): String = self.display(f(value))
  }
}

object Display {
  def apply[A](value: A)(using d: Display[A]): String = d.display(value)
}
