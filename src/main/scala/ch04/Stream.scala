package ch04

trait Stream[A] { self =>
  def head: A
  def tail: Stream[A]

  def take(n: Int): List[A] =
    if n <= 0 then Nil
    else head :: tail.take(n - 1)

  def map[B](f: A => B): Stream[B] =
    new Stream[B] {
      def head: B         = f(self.head)
      def tail: Stream[B] = self.tail.map(f)
    }

  def filter(p: A => Boolean): Stream[A] =
    new Stream[A] {
      def head: A =
        if p(self.head) then self.head
        else self.tail.filter(p).head
      def tail: Stream[A] =
        if p(self.head) then self.tail.filter(p)
        else self.tail.tail.filter(p)
    }

  def zip[B](that: Stream[B]): Stream[(A, B)] =
    new Stream[(A, B)] {
      def head: (A, B)         = self.head -> that.head
      def tail: Stream[(A, B)] = self.tail.zip(that.tail)
    }

  def scanLeft[B](z: B)(f: (B, A) => B): Stream[B] =
    new Stream[B] {
      def head: B         = f(z, self.head)
      def tail: Stream[B] = self.tail.scanLeft(f(z, self.head))(f)
    }
}

object Stream {
  def const[A](a: A): Stream[A] =
    new Stream[A] {
      def head: A         = a
      def tail: Stream[A] = const(a)
    }

  def unfold[A, B](seed: A, f: A => B, next: A => A): Stream[B] =
    new Stream[B] {
      def head: B         = f(seed)
      def tail: Stream[B] = unfold(next(seed), f, next)
    }

  def unfoldByNeed[A, B](seed: A, f: A => B, next: A => A): Stream[B] =
    new Stream[B] {
      lazy val head: B         = f(seed)
      lazy val tail: Stream[B] = unfoldByNeed(next(seed), f, next)
    }
}
