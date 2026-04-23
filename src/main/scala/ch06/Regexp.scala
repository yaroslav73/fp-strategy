package ch06

import scala.annotation.tailrec

enum Regexp {
  def ++(that: Regexp): Regexp     = Append(this, that)
  def orElse(that: Regexp): Regexp = OrElse(this, that)
  def repeat: Regexp               = Repeat(this)
  def `*`: Regexp                  = this.repeat

  def matches(input: String): Boolean = {
    type Continuation = Option[Int] => Call

    enum Call {
      case Loop(regexp: Regexp, index: Int, c: Continuation)
      case Continue(nextIndex: Option[Int], c: Continuation)
      case Done(index: Option[Int])
    }

    def loop(regexp: Regexp, index: Int, c: Continuation): Call =
      regexp match {
        case Append(left, right) =>
          val nextC: Continuation = {
            case None            => Call.Continue(None, c)
            case Some(nextIndex) => Call.Loop(right, nextIndex, c)
          }
          Call.Loop(left, index, nextC)
        case OrElse(first, second) =>
          val nextC: Continuation = {
            case None            => Call.Loop(second, index, c)
            case Some(nextIndex) => Call.Continue(Some(nextIndex), c)
          }
          Call.Loop(first, index, nextC)
        case Repeat(pattern) =>
          val nextC: Continuation = {
            case None            => Call.Continue(Some(index), c)
            case Some(nextIndex) => Call.Loop(regexp, nextIndex, c)
          }
          Call.Loop(pattern, index, nextC)
        case Apply(s) => Call.Continue(Option.when(input.startsWith(s, index))(index + s.length), c)
        case Empty    => Call.Continue(None, c)
      }

    @tailrec
    def trampoline(next: Call): Option[Int] =
      next match {
        case Call.Loop(regexp, index, c) => trampoline(loop(regexp, index, c))
        case Call.Continue(nextIndex, c) => trampoline(c(nextIndex))
        case Call.Done(index)            => index
      }

    trampoline(loop(this, 0, opt => Call.Done(opt))).contains(input.length)
  }

  case Append(left: Regexp, right: Regexp)
  case OrElse(first: Regexp, second: Regexp)
  case Repeat(pattern: Regexp)
  case Apply(s: String)
  case Empty
}

object Regexp {
  val empty: Regexp = Empty

  def apply(s: String): Regexp = Apply(s)
}
