import cats._
import cats.data._
import cats.implicits._
import cats.mtl.Tell

case class ServiceParams(option1: String, option2: Int)

case class ServiceResult(userId: Int, companies: List[String])

def serviceCall[F[_]: Monad](params: ServiceParams): F[ServiceResult] =
// a fake call to some external service, replace with real implementation
  ServiceResult(0, List("Raven Enterprises")).pure[F]