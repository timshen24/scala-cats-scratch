package ch1.typeclass

import cats.instances.string._
import cats.syntax.semigroup._
import JsonSyntax._
import JsonWriter._

sealed trait Json
final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json
case object JsNull extends Json

trait JsonWriter[A] {
  def write(value: A): Json
}

final case class Person(name: String, email: String)

object JsonWriter {
  implicit val stringWriter: JsonWriter[String] =
    (value: String) => JsString(value)

  implicit val personWriter: JsonWriter[Person] =
    (value: Person) => JsObject(Map(
      "name" -> JsString(value.name),
      "email" -> JsString(value.email)
    ))
}

object JsonSyntax {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
    w.write(value)

  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json =
      w.write(value) // == w.write(value)
  }

  implicit def optionWriter[A: JsonWriter]: JsonWriter[Option[A]] =
  {
    case Some(value) => implicitly[JsonWriter[A]].write(value)
    case None => JsNull
  }
}

object Main extends App {

}
