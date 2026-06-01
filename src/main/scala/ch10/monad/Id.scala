package ch10.monad

type Id[A] = A
object Id:
  def apply[A](a: A): Id[A] = a
