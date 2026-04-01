package ch04

import ch04.Bool.{ False, True }

trait Bool { self =>
  def iff[A](t: A)(f: A): A

  def and[A](that: => Bool): Bool =
    self.iff(that)(False)

  def or[A](that: => Bool): Bool =
    self.iff(True)(that)

  def not: Bool =
    self.iff(False)(True)
}

object Bool {
  val True: Bool = new Bool {
    def iff[A](t: A)(f: A): A        = t
    def fold[A](t: => A)(f: => A): A = t
  }

  val False: Bool = new Bool {
    def iff[A](t: A)(f: A): A        = f
    def fold[A](t: => A)(f: => A): A = f
  }
}
