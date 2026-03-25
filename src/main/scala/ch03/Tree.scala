package ch03

enum Tree[A] { self =>
  case Leaf(value: A)
  case Node(left: Tree[A], right: Tree[A])

  def map[B](f: A => B): Tree[B] =
    self.fold(v => Leaf(f(v)))((l, r) => Node(l, r))

  def size: Int =
    self.fold(0)((_, acc) => acc + 1)

  def contains(elem: A): Boolean =
    self.fold(false)((a, b) => a == elem || b)

  def fold[B](zero: B)(f: (A, B) => B): B =
    self match
      case Leaf(value)       => f(value, zero)
      case Node(left, right) => right.fold(left.fold(zero)(f))(f)

  // author solution
  def fold[B](leaf: A => B)(node: (B, B) => B): B =
    self match
      case Leaf(v)           => leaf(v)
      case Node(left, right) => node(left.fold(leaf)(node), right.fold(leaf)(node))
}

object Tree {
  def apply[A](value: A): Tree[A] = Leaf(value)

  def apply[A](value: Tree[A]): Tree[A] = value

  def apply[A](left: Tree[A], right: Tree[A]): Tree[A] = Node(left, right)
}
