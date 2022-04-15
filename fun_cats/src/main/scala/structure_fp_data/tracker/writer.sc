import cats._
import cats.data._
import cats.implicits._

type Tracked[A] = Writer[List[String], A]

8.pure[Tracked]
List("hello").tell
val x = 10.writer(List("a ten"))
Writer(List("an eight"), 8)

x.reset
x.listen
x.value
x.run

x.map(_ + 1)
x.flatMap(i => (i * 2).writer(List("multiplied by two")))

val y = 15.writer(List("a fifteen"))
(x, y).mapN(_ + _)

val z = 20.writer(List("a twenty"))
x |+| y

List(x, y, z).combineAll