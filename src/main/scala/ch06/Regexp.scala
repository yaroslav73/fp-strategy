package ch06

enum Regexp {
  def ++(that: Regexp): Regexp     = Append(this, that)
  def orElse(that: Regexp): Regexp = OrElse(this, that)
  def repeat: Regexp               = Repeat(this)
  def `*`: Regexp                  = this.repeat

  def matches(input: String): Boolean = {
    type Continuation = Option[Int] => Option[Int]

    def loop(regexp: Regexp, index: Int, c: Continuation): Option[Int] =
      regexp match {
        case Append(left, right) =>
          val nextC: Continuation = {
            case None            => c(None)
            case Some(nextIndex) => loop(right, nextIndex, c)
          }
          loop(left, index, nextC)
        case OrElse(first, second) =>
          val nextC: Continuation = {
            case None            => loop(second, index, c)
            case Some(nextIndex) => c(Some(nextIndex))
          }
          loop(first, index, nextC)
        case Repeat(pattern) =>
          val nextC: Continuation = {
            case None            => c(Some(index))
            case Some(nextIndex) => loop(regexp, nextIndex, c)
          }
          loop(pattern, index, nextC)
        case Apply(s) => c(Option.when(input.startsWith(s, index))(index + s.length))
        case Empty    => c(None)
      }

    loop(this, 0, identity).contains(input.length)
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
