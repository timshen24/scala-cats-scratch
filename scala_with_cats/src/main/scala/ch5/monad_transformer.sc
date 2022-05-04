import cats.data._
import cats.implicits._
type ListOption[A] = OptionT[List, A]
import cats.instances.list._

val result1: ListOption[Int] = OptionT(List(Option(10)))
val result2: ListOption[Int] = 32.pure[ListOption]

result1.flatMap { (x: Int) =>
  result2.map {
    (y: Int) => x + y
  }
}

for {
  x <- result1
  y <- result2
} yield x + y