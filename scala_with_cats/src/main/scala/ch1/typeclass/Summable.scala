package ch1.typeclass

import ch1.typeclass.Summable.processMyList

trait Summable[T] {
  def sumElements(list: List[T]): T
}

object Summable {
  implicit object IntSummable extends Summable[Int] {
    def sumElements(list: List[Int]): Int = list.sum
  }

  implicit object StringSummable extends Summable[String] {
    def sumElements(list: List[String]): String = list.mkString("")
  }

  def processMyList[T](list: List[T])(implicit summable: Summable[T]): T =
    summable.sumElements(list)
}

object Main {
  def main(args: Array[String]): Unit = {
    println(processMyList(List(1, 2, 3))) // 6
    println(processMyList(List("Scala ", "is ", "awesome"))) // "Scala is awesome"
    //    processMyList(List(true, true, false))
  }
}






