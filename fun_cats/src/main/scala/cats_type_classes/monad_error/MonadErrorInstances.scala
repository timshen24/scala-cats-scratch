package cats_type_classes.monad_error

import cats._
import cats.implicits._

import scala.util._

object MonadErrorInstances {
  def eitherME[E]: MonadError[Either[E, *], E] = new MonadError[Either[E, *], E] {
    override def pure[A](a: A): Either[E, A] = Right(a)

    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa match {
      case Left(e) => Left(e)
      case Right(value) => f(value)
    }

    override def tailRecM[A, B](a: A)(f: A => Either[E, Either[A, B]]): Either[E, B] = ???

    override def raiseError[A](e: E): Either[E, A] = Left(e)

    override def handleErrorWith[A](fa: Either[E, A])(f: E => Either[E, A]): Either[E, A] = fa match {
      case Left(e) => f(e)
      case Right(value) => Right(value)
    }
  }

  val tryME: MonadError[Try, Throwable] = new MonadError[Try, Throwable] {
    override def pure[A](a: A): Try[A] = Success(a)

    override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] =
      fa match {
        case Failure(exception) => Failure(exception)
        case Success(value) => f(value)
      }

    override def tailRecM[A, B](a: A)(f: A => Try[Either[A, B]]): Try[B] = ???

    override def raiseError[A](e: Throwable): Try[A] = Failure(e)

    override def handleErrorWith[A](fa: Try[A])(f: Throwable => Try[A]): Try[A] = fa match {
      case Failure(exception) => f(exception)
      case Success(value) => Success(value)
    }
  }
}
