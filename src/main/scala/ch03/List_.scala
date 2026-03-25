package ch03

import ch03.List_.unfold

enum List_[A] { self =>
  case Empty()
  case Pair(_head: A, _tail: List_[A])

  def isEmpty: Boolean =
    self match
      case Empty() => true
      case _       => false

  private def head: A =
    self match
      case Pair(h, _) => h
      case _          => throw new NoSuchElementException("head of empty list")

  def headOpt: Option[A] =
    self match
      case Empty()    => None
      case Pair(h, _) => Some(h)

  def tail: List_[A] =
    self match
      case Empty()       => Empty()
      case Pair(_, tail) => tail

  def map[B](f: A => B): List_[B] =
    unfold(self)(_.isEmpty, l => f(l.head), _.tail)
}

object List_ {
  def unfold[A, B](seed: A)(stop: A => Boolean, f: A => B, next: A => A): List_[B] =
    if stop(seed) then Empty()
    else Pair(f(seed), unfold(next(seed))(stop, f, next))

  def fill[A](n: Int)(elem: => A): List_[A] =
    unfold(0)(_ >= n, _ => elem, _ + 1)

  def iterate[A](start: A, len: Int)(f: A => A): List_[A] =
    unfold((start, 0))((_, n) => n >= len, (a, _) => a, (a, n) => (f(a), n + 1))
}
