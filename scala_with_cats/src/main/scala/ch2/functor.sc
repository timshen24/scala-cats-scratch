import cats.Functor
import cats.instances.list._
import cats.instances.option._
import cats.implicits._

val list1 = List(1, 2, 3)
Functor[List].map(list1)(_ * 2)

Functor[Option].map(Option(123))(_.toString)

val func = (x: Int) => x + 1
val liftedFunc = Functor[Option].lift(func)
liftedFunc(Option(1))

Functor[List].as(list1, "As")

def doMath[F[_]: Functor](start: F[Int]): F[Int] = {
  start.map(n => n + 1 * 2)
}

doMath(Option(20))
doMath(List(1, 2, 3))


import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

val future: Future[String] =
  Future(123).map(n => n + 1)
    .map(_ * 2)
    .map(n => s"$n!")
println(Await.result(future, 1.second))