package ch05

// Rule 1:
// We should know about the given scope is that it
// starts at the call site, where the method with a using clause is
// called, not at the definition site where the method is defined.
object A1 {
  given a: Int = 10

  def whichInt(using n: Int): Int = n
}

@main def testA1(): Unit = {
  // No given instance of type Int was found for
  // parameter n of method whichInt in object A
  // The following import might fix the problem:
  //  import ch05.A1.a

//  A1.whichInt
}

// Rule 2:
// which we have been relying on in all our examples so far,
// is that the given scope includes the lexical scope at the call site.
object A2 {
  given a: Int = 10

  object B2 {
    def whichInt(using n: Int): Int = n
  }

  object C2 {
    B2.whichInt
  }
}

object A3 {
  given a: Int = 10
  given b: Int = 20

  def whichInt(using n: Int): Int = n

  // Method call below does not compile because of ambiguity:
  // Ambiguous given instances:
  // both given instance a in object A3 and given instance b in object A3
  // match type Int of parameter n of method whichInt in object A3

//  whichInt
}

object A4 {
  given a: Int = 10

  def whichInt(using n: Int): Int = n
}

object B4 {
  import ch05.A4.*

  // Code below also does not compile because of ambiguity:
  // No given instance of type Int was found for parameter n of method whichInt in object A4
  // Note: given instance a in object A4 was not considered because it was not imported with `import given`.

//  whichInt
}

// It will work if
object A5 {
  given a: Int = 10

  def whichInt(using n: Int): Int = n
}

object B5 {
  import ch05.A5.{ *, given }

  whichInt
}

// Rule 3:
// the given scope includes the companion objects
// of any type involved in the type of the using clause.
trait Sound[A] {
  def sound: String
}

object Sound {
  def soundOf[A](using s: Sound[A]): String = s.sound
}

trait Cat
object Cat {
  given catSound: Sound[Cat] with
    def sound: String = "meow"
}

trait Dog
object Dog {
  given dogSound: Sound[Dog] with
    def sound: String = "woof"
}

@main def testSound(): Unit = {
  println(Sound.soundOf[Cat])
  println(Sound.soundOf[Dog])
}

// The first rule is that explicitly passing
// an instance takes priority over everything else.
@main def testPriority01(): Unit =
  given a: Int                      = 1
  def whichInt(using int: Int): Int = int
  println(whichInt(using 2))

// The second rule is that instances in the lexical scope
// take priority over instances in a companion object.
@main def testPriority02(): Unit = {
  given purr: Sound[Cat] with
    def sound: String = "purr"

  println(Sound.soundOf[Cat])
}

// The final rule is that instances in a closer lexical scope
// take preference over those further away.
@main def testPriority03(): Unit = {
  given purr: Sound[Cat] with
    def sound: String = "purr"

  {
    given meow: Sound[Cat] with
      def sound: String = "mew"

    println(Sound.soundOf[Cat])
  }
}
