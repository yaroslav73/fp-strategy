package ch05.json

enum Json {
  case JsObject(fields: Map[String, Json])
  case JsArray(elements: List[Json])
  case JsString(value: String)
  case JsNumber(value: Double)
  case JsBoolean(value: Boolean)
  case JsNull
}

object Json {
  def toJson[A](value: A)(using writer: JsonWriter[A]): Json =
    writer.write(value)
    
  object Syntax {
    extension [A](value: A)
      def toJson(using writer: JsonWriter[A]): Json =
        writer.write(value)
  }
}
