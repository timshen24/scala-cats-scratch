import cats.Traverse
import cats.instances.future._
import cats.instances.list._

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

val totalUptime: Future[List[Int]] = Traverse[List].traverse(hostnames)(getUptime)

Await.result(totalUptime, 1.second)

val numbers = List(Future(1), Future(2), Future(3))

val numbers2: Future[List[Int]] = Traverse[List].sequence(numbers)

Await.result(numbers2, 1.second)

import cats.syntax.traverse._
Await.result(hostnames.traverse(getUptime), 1.second)
Await.result(numbers.sequence, 1.second)