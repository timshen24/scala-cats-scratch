import cats.Eq
import cats.instances.int._
import cats.syntax.eq._
import cats.implicits._

import java.util.Date

val eqInt = Eq[Int]
eqInt.eqv(123, 234)

123===234
345===345

1.some === none[Int]
1.some =!= none[Int]

implicit val dateEq: Eq[Date] = Eq.instance[Date] {
  (d1, d2) => d1.getTime === d2.getTime
}

val x = new Date
val y = new Date
x === y

final case class Cat(name: String, age: Int, color: String)
implicit val catEq: Eq[Cat] = Eq.instance {
  (cat1, cat2) => cat1.name === cat2.name && cat1.age === cat2.age && cat1.color === cat2.color
}

val cat1 = Cat("Garfield",   38, "orange and black")
val cat2 = Cat("Heathcliff", 33, "orange and black")
println(cat1 === cat2)

val optionCat1 = Option(cat1)
val optionCat2 = Option.empty[Cat]
println(optionCat1 === optionCat2)
println(optionCat1 =!= optionCat2)