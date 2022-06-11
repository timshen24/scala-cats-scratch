import cats.effect._
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent._

IO.pure(12)
IO.raiseError(new RuntimeException("Oh noes!"))
def futurish: Future[String] = Future("Hello")
IO.fromFuture(IO(futurish))

IO(12).map(_ + 1)
(IO(12), IO("hi")).mapN((i, s) => s"$s: $i")
val added = for {
  i <- IO(12)
  j <- IO(i + 1)
} yield i + j

val ohNoes = IO.raiseError[Int](new RuntimeException("Oh noes!"))
val handled: IO[Int] = ohNoes.handleErrorWith(_ => IO(12))
ohNoes.handleError(_ => 12)
val handled: IO[Int] = ohNoes.handleErrorWith(_ => IO.raiseError(new RuntimeException("other exception")))
ohNoes.adaptError(t => new RuntimeException(t.getMessage))

val attempted: IO[Either[Throwable, Int]] = ohNoes.attempt

import cats.effect.unsafe.implicits.global
IO("hello world").unsafeRunSync()
added.unsafeRunSync()