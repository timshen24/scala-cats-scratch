package ch1.typeclass

import cats.Show
import cats.implicits._

import java.time.LocalDate

object CatsShow2 {
  final case class Cat(name: String, age: Int, color: String)

  implicit val dateShow: Show[LocalDate] = Show.show(date => s"${date.toString}ms since the epoch")
  implicit val catShow: Show[Cat] = Show.show(cat => s"${cat.name.show} is a ${cat.age.show} year-old ${cat.color.show} cat.")

  def main(args: Array[String]): Unit = {
    println(LocalDate.now.show)
    println(Cat("Garfield", 38, "ginger and black").show)
  }
}
