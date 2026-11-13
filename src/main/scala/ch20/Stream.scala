package ch20

import scala.annotation.tailrec

enum Stream[A]:
  case Map[A, B](source: Stream[A], f: A => B) extends Stream[B]
  case FlatMap[A, B](source: Stream[A], f: A => Stream[B]) extends Stream[B]
  case Product[A, B](left: Stream[A], right: Stream[B]) extends Stream[(A, B)]
  case Take(source: Stream[A], count: Int) extends Stream[A]
  case Merge(left: Stream[A], right: Stream[A]) extends Stream[A]
  case Filter(source: Stream[A], pred: A => Boolean) extends Stream[A]
  case FromIterator(it: Iterator[A]) extends Stream[A]
  case FromSeq(seq: Seq[A]) extends Stream[A]

  def map[B](f: A => B): Stream[B] = Map(this, f)

  def flatMap[B](f: A => Stream[B]): Stream[B] = FlatMap(this, f)

  def product[B](that: Stream[B]) = Product(this, that)

  def foldLeft[B](zero: B)(f: (B, A) => B): B =
    import Stream.{ Compiled, Emit }

    val compiled = Compiled.fromStream(this)

    @tailrec
    def loop(zero: B): B =
      compiled.next match
        case Emit.Value(v) => loop(f(zero, v))
        case Emit.Wait     => loop(zero)
        case Emit.End      => zero

    loop(zero)

  def take(count: Int): Stream[A] = Take(this, count)

  def merge(that: Stream[A]): Stream[A] = Merge(this, that)

  def filter(pred: A => Boolean): Stream[A] = Filter(this, pred)

  def toSeq: Seq[A] = foldLeft(Seq.empty)(_ :+ _)

object Stream:
  enum Emit[+A]:
    case Value(v: A) // The pull produce a value
    case Wait // There is no value available now, but there may be in the future
    case End // The Stream has ended and no values will ever be available

    def map[B](f: A => B): Emit[B] =
      this match
        case Value(v) => Value(f(v))
        case Wait     => Wait
        case End      => End

  enum Pos:
    case Left, Right

  enum ProductState[+A, +B]:
    case Continue // Pull from both left and righ
    case CacheLeft(left: A) // Left value is cached. Pull from right.
    case CacheRight(right: B) // Right value is cached. Pull from left.
    case End

  enum Compiled[A]:
    case Map[A, B](source: Compiled[A], f: A => B) extends Compiled[B]
    case FlatMap[A, B](source: Compiled[A], f: A => Compiled[B], var state: Option[Compiled[B]]) extends Compiled[B]
    case Product[A, B](left: Compiled[A], right: Compiled[B], var state: ProductState[A, B]) extends Compiled[(A, B)]
    case Take(source: Compiled[A], count: Int, var taken: Int = 0) extends Compiled[A]
    case Merge(left: Compiled[A], right: Compiled[A], var takeFrom: Pos = Pos.Left) extends Compiled[A]
    case Filter(source: Compiled[A], pred: A => Boolean) extends Compiled[A]
    case FromIterator(it: Iterator[A]) extends Compiled[A]
    case FromSeq(seq: Seq[A], var idx: Int = 0) extends Compiled[A]

    def next: Emit[A] =
      this match {
        case Compiled.Map(source, f) => source.next.map(f)
        case c @ Compiled.FlatMap(source, f, state) =>
          state match {
            case Some(s) =>
              s.next match {
                case Emit.End =>
                  c.state = None
                  Emit.Wait
                case other => other
              }
            case None =>
              source.next match {
                case Emit.Value(v) =>
                  val newState = f(v)
                  c.state = Some(newState)
                  Emit.Wait
                case Emit.Wait => Emit.Wait
                case Emit.End  => Emit.End
              }
          }
        case c @ Compiled.Product(left, right, state) =>
          state match {
            case ProductState.Continue =>
              (left.next, right.next) match
                case (Emit.End, _) =>
                  c.state = ProductState.End
                  Emit.End
                case (_, Emit.End) =>
                  c.state = ProductState.End
                  Emit.End
                case (Emit.Wait, Emit.Wait) =>
                  Emit.Wait
                case (Emit.Wait, Emit.Value(r)) =>
                  c.state = ProductState.CacheRight(r)
                  Emit.Wait
                case (Emit.Value(l), Emit.Wait) =>
                  c.state = ProductState.CacheLeft(l)
                  Emit.Wait
                case (Emit.Value(l), Emit.Value(r)) =>
                  Emit.Value((l, r))
            case ProductState.CacheLeft(l) =>
              right.next match
                case Emit.Value(r) =>
                  c.state = ProductState.Continue
                  Emit.Value((l, r))
                case Emit.Wait =>
                  Emit.Wait
                case Emit.End =>
                  c.state = ProductState.End
                  Emit.End
            case ProductState.CacheRight(r) =>
              left.next match
                case Emit.Value(l) =>
                  c.state = ProductState.Continue
                  Emit.Value((l, r))
                case Emit.Wait =>
                  Emit.Wait
                case Emit.End =>
                  c.state = ProductState.End
                  Emit.End
            case ProductState.End => Emit.End
          }
        case c @ Compiled.Take(source, count, taken) =>
          if taken == count then Emit.End
          else
            source.next match
              case v @ Emit.Value(_) =>
                c.taken = taken + 1
                v
              case other => other
        case c @ Compiled.Merge(left, right, takeFrom) =>
          takeFrom match
            case Pos.Left =>
              c.takeFrom = Pos.Right
              left.next
            case Pos.Right =>
              c.takeFrom = Pos.Left
              right.next
        case Compiled.Filter(source, pred) =>
          source.next match
            case Emit.Value(v) if pred(v) => Emit.Value(v)
            case Emit.Value(_)            => Emit.Wait
            case Emit.Wait                => Emit.Wait
            case Emit.End                 => Emit.End
        case Compiled.FromIterator(it) =>
          if it.hasNext then Emit.Value(it.next) else Emit.End
        case c @ Compiled.FromSeq(seq, idx) =>
          if idx == seq.size then Emit.End
          else
            val elem = seq(idx)
            c.idx = idx + 1
            Emit.Value(elem)
      }

  object Compiled:
    def fromStream[A](stream: Stream[A]): Compiled[A] =
      stream match {
        case Stream.Map(source, f)       => Map(fromStream(source), f)
        case Stream.FlatMap(source, f)   => FlatMap(fromStream(source), a => fromStream(f(a)), state = None)
        case Stream.Product(left, right) => Product(fromStream(left), fromStream(right), ProductState.Continue)
        case Stream.Take(source, count)  => Take(fromStream(source), count)
        case Stream.Merge(left, right)   => Merge(fromStream(left), fromStream(right))
        case Stream.Filter(source, pred) => Filter(fromStream(source), pred)
        case Stream.FromIterator(it)     => FromIterator(it)
        case Stream.FromSeq(seq)         => FromSeq(seq)
      }

  def fromIterator[A](it: Iterator[A]): Stream[A] = FromIterator(it)

  def fromSeq[A](seq: Seq[A]): Stream[A] = FromSeq(seq)
