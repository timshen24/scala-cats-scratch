import cats._
import cats.implicits._
import scala.util._

implicit val tryMonad: Monad[Try] = new Monad[Try] {
  override def pure[A](x: A): Try[A] = Success(x)

  override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] =
    fa match {
      case Success(value) => f(value)
      case Failure(exception) => Failure(exception)
    }

  override def tailRecM[A, B](a: A)(f: A => Try[Either[A, B]]): Try[B] = ???
}

tryMonad.pure(5)
tryMonad.pure(5).flatMap(i => tryMonad.pure(i + 1))
tryMonad.pure(5).flatMap(_ => Failure(new Exception("boom")))
tryMonad.pure(5)
  .flatMap((_: Int) => Failure(new Exception("boom"))
    .flatMap((_: Int) => Failure(new Exception("boom2"))))
