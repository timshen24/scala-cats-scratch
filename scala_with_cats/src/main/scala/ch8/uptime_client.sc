import cats._
import cats.implicits._

import scala.concurrent.Future

trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}

trait RealUptimeClient extends UptimeClient[Future] {
//  override def getUptime(hostname: String): Future[Int] = ???
}

val hosts = Map("host1" -> 10, "host2" -> 6)

class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
  def getUptime(hostname: String): Id[Int] =
    hosts.getOrElse(hostname, 0)
}

class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)
}

val client   = new TestUptimeClient(hosts)
val service  = new UptimeService(client) // Id as context, that is to say F = Id
val actual   = service.getTotalUptime(hosts.keys.toList)
val expected = hosts.values.sum
assert(actual == expected)
