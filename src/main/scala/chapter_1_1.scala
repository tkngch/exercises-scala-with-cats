package tkngch.scalawithcats.chapter_1_1

import cats.instances.string._
import cats.syntax.semigroup._

// Examples in the chapter.

// Define a very simple JSON AST
sealed trait Json
final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json
case object JsNull extends Json

// The "serialize to JSON" behaviour is encoded in this trait
trait JsonWriter[A] {
  def write(value: A): Json
}

final case class Person(name: String, email: String)

object JsonWriterInstances {
  implicit val stringWriter: JsonWriter[String] =
    new JsonWriter[String] {
      def write(value: String): Json =
        JsString(value)
    }

  implicit val personWriter: JsonWriter[Person] =
    new JsonWriter[Person] {
      def write(value: Person): Json =
        JsObject(
          Map(
            "name" -> JsString(value.name),
            "email" -> JsString(value.email)
          )
        )
    }
}

// Interface Objects
object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
    w.write(value)
}

// Interface Syntax
object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json =
      w.write(value)
  }
}

object Main extends App {
  import JsonWriterInstances._
  import JsonSyntax._

  println("Hello " |+| "Cats!")
  val json = Json.toJson(Person("Dave", "dave@example.com"))
  println(json)
  Person("Dave", "dave@example.com").toJson
}
