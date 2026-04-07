package ch05.json

@main def runJsonExamples(): Unit =
  final case class Person(name: String, email: String)

  given personWriter: JsonWriter[Person] with
    def write(value: Person): Json =
      Json.JsObject(
        Map(
          "name" -> Json.JsString(value.name),
          "email" -> Json.JsString(value.email)
        )
      )

  val alice = Person("Alice", "alice@test.com")
  println(personWriter.write(alice))
  println(Json.toJson(alice))

  import Json.Syntax.*
  println(alice.toJson)

  // The summon method:
  // val s = summon[JsonWriter[String]]

  val aliceOpt: Option[Person]  = Option(alice)
  val personOpt: Option[Person] = None
  println(aliceOpt.toJson) // Error: No given JsonWriter[Option[Person]] found
  println(personOpt.toJson)
