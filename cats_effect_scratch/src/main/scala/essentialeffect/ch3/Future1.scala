package essentialeffect.ch3

import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

/**
 * This program will only print one pair of hello world. Because Future is so eager it runs at once before the second pair can get its value.
 */
object Future1 extends App {
  val hello = Future(println(s"[${Thread.currentThread.getName}] Hello"))
  val world = Future(println(s"[${Thread.currentThread.getName}] World"))

  val hw1: Future[Unit] =
    for {
      _ <- hello
      _ <- world
    } yield ()

  Await.ready(hw1, 5.seconds)

  val hw2: Future[Unit] = (hello, world).mapN((_, _) => ())

  Await.ready(hw2, 5.seconds)
}
