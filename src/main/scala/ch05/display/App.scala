package ch05.display

@main def runDisplayExamples(): Unit =
  import Display.*
  import DisplaySyntax.*
  
  final case class Cat(name: String, age: Int, color: String)
  object Cat {
    given catDisplay: Display[Cat] with
      def display(value: Cat): String =
        s"${value.name} is a ${value.age} year-old ${value.color} cat."
  }
  
  val cat = Cat("Whiskers", 3, "black")
  cat.print