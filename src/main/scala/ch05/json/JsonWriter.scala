package ch05.json

trait JsonWriter[A] {
  def write(value: A): Json
}

object JsonWriter {
  given stringWriter: JsonWriter[String] with
    def write(value: String): Json =
      Json.JsString(value)

  given intWriter: JsonWriter[Int] with
    def write(value: Int): Json =
      Json.JsNumber(value)

  given optionWriter[A](using writer: JsonWriter[A]): JsonWriter[Option[A]] with
    def write(value: Option[A]): Json =
      value match
        case Some(v) => writer.write(v)
        case None    => Json.JsNull
}
