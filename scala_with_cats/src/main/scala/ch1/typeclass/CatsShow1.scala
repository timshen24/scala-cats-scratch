package ch1.typeclass

import cats.Show
import cats.implicits._

object CatsShow1 {
  def main(args: Array[String]): Unit = {
    val showInt: Show[Int] = Show.apply[Int]
    val showString: Show[String] = Show.apply[String]
    println(showInt.show(123))
    println(showString.show("abc"))

    println(123.show)
    println("student".show)
  }
}