package ch20

import cats.syntax.all.*

import scala.None
import scala.annotation.tailrec

enum Stream[A]:
  case Map[A, B](source: Stream[A], f: A => B) extends Stream[B]
  case Product[A, B](left: Stream[A], right: Stream[B]) extends Stream[(A, B)]
  case FromIterator(it: Iterator[A])
  case FromSeq(seq: Seq[A])

  def map[B](f: A => B): Stream[B] = Map(this, f)

  def product[B](that: Stream[B]) = Product(this, that)

  def foldLeft[B](zero: B)(f: (B, A) => B): B =
    import Stream.Compiled

    val compiled = Stream.Compiled.fromStream(this)

    def next[C](compiled: Compiled[C]): Option[C] =
      compiled match {
        case Compiled.Map(source, f) => next(source).map(f)
        case Compiled.Product(left, right) => (next(left), next(right)).tupled
        case Compiled.FromIterator(it) => it.nextOption()
        case c@Compiled.FromSeq(seq, idx) =>
          if idx == seq.size then None
          else
            val elem = seq(idx)
            c.idx = idx + 1
            Some(elem)
      }

    @tailrec
    def loop(zero: B): B =
      next(compiled) match
        case Some(value) => loop(f(zero, value))
        case None => zero

    loop(zero)
    
  def take(count: Int): Stream[A] = ???

  def toSeq: Seq[A] = foldLeft(Seq.empty)(_ :+ _)


object Stream:
  enum Compiled[A]:
    case Map[A, B](source: Compiled[A], f: A => B) extends Compiled[B]
    case Product[A, B](left: Compiled[A], right: Compiled[B]) extends Compiled[(A, B)]
    case FromIterator(it: Iterator[A])
    case FromSeq(seq: Seq[A], var idx: Int = 0)

  object Compiled:

    import Compiled.*

    def fromStream[A](stream: Stream[A]): Compiled[A] =
      stream match {
        case Stream.Map(source, f) => Map(fromStream(source), f)
        case Stream.Product(left, right) => Product(fromStream(left), fromStream(right))
        case Stream.FromIterator(it) => FromIterator(it)
        case Stream.FromSeq(seq) => FromSeq(seq)
      }

  def fromIterator[A](it: Iterator[A]): Stream[A] = FromIterator(it)

  def fromSeq[A](seq: Seq[A]): Stream[A] = FromSeq(seq)