import cats.{MonadError, MonadThrow}
import cats.instances.either._
import cats.syntax.applicativeError._
import cats.syntax.monadError._
import cats.implicits._

import scala.util.Try

type ErrorOr[A] = Either[String, A]
val monadError: MonadError[ErrorOr, String] = MonadError[ErrorOr, String]

42.pure[ErrorOr]
val success = monadError.pure(42)
val failure = monadError.raiseError("Badness")

failure.handleErrorWith {
  case "Badness" => monadError.pure("It's OK")
  case _ => "It's not OK".raiseError
}

success.ensure("Number too low!")(_ > 1000)

def validateAdult[F[_]: MonadThrow](age: Int)/*(implicit me: MonadError[F, Throwable])*/: F[Int] = {
  val monadError: F[Int] = age.pure[F]
  monadError.ensure(new IllegalArgumentException("Age too young"))(_ >= 18)
}

validateAdult[Try](18)
validateAdult[Try](8)
type ExceptionOr[A] = Either[Throwable, A]
validateAdult[ExceptionOr](-1)