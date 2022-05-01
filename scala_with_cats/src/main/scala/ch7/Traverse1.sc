import cats.Applicative

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

val hostnames = List(
  "alpha.example.com",
  "beta.example.com",
  "gamma.demo.com"
)

def getUptime(hostname: String): Future[Int] =
  Future(hostname.length * 60)

getUptime(hostnames.head)
getUptime(hostnames(1))
getUptime(hostnames(2))

/*val allUptimes: Future[List[Int]] =
  hostnames.foldLeft(Future(List.empty[Int])) {
    (accum, host) =>
      val uptime = getUptime(host)
      for {
        accum <- accum
        uptime <- uptime
      } yield accum :+ uptime
  }*/
// Future.traverse and Future.sequence solve a very specific problem: they allow us to iterate over a sequence of Futures and accumulate a result.
val allUptimes: Future[List[Int]] = Future.traverse(hostnames)(getUptime)
Await.result(allUptimes, 1.second)

import cats.instances.future._
import cats.instances.option._
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.apply._

def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
  list.foldRight(List.empty[B].pure[F]) {
    (a, acc) => (func(a), acc).mapN(_ :: _)
  }

def listSequence[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] =
  listTraverse(list)(identity)

val totalUptime = listTraverse(hostnames)(getUptime)

listSequence(List(Vector(1, 2), Vector(3, 4)))
listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))

def process(inputs: List[Int]) =
  listTraverse(inputs)(n => if(n % 2 == 0) Some(n) else None)

process(List(2, 4, 6))
process(List(1, 2, 3))

import cats.data.Validated
import cats.instances.list._

type ErrorsOr[A] = Validated[List[String], A]

def process(inputs: List[Int]): ErrorsOr[List[Int]] =
  listTraverse(inputs) { n =>
    if(n % 2 == 0) {
      Validated.valid(n)
    } else {
      Validated.invalid(List(s"$n is not even"))
    }
  }
process(List(2, 4, 6))
process(List(1, 2, 3))

import cats.syntax.traverse._
Await.result(hostnames.traverse(getUptime), 1.second)
