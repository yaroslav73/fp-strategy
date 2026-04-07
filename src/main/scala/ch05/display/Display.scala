package ch05.display

trait Display[A] {
  def display(value: A): String
}

object Display {
  def display[A](value: A)(using d: Display[A]): String =
    d.display(value)
  
  def print[A](value: A)(using d: Display[A]): Unit =
    println(d.display(value))
  
  given intDisplay: Display[Int] with
    def display(value: Int): String =
      value.toString
      
  given stringDisplay: Display[String] with
    def display(value: String): String = 
      value
}
