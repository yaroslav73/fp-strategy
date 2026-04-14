package ch06

enum Regexp {
  def ++(that: Regexp): Regexp     = Append(this, that)
  def orElse(that: Regexp): Regexp = OrElse(this, that)
  def repeat: Regexp               = Repeat(this)
  def `*`: Regexp                  = this.repeat

  def matches(input: String): Boolean = {
    def loop(regexp: Regexp, index: Int): Option[Int] =
      regexp match {
        case Append(left, right)   => loop(left, index).flatMap(nextIndex => loop(right, nextIndex))
        case OrElse(first, second) => loop(first, index).orElse(loop(second, index))
        case Repeat(pattern) =>
          loop(pattern, index)
            .flatMap(nextIndex => loop(regexp, nextIndex))
            .orElse(Some(index))
        case Apply(s) => Option.when(input.startsWith(s, index))(index + s.length)
        case Empty    => None
      }

    loop(this, 0).contains(input.length)
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
