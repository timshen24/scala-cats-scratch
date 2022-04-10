package cats_type_classes.monad

import cats.Monad
import cats.implicits._

object MonadEither {
  implicit def eitherMonad[E]: Monad[Either[E, *]] = new Monad[Either[E, *]] {
    override def pure[A](a: A): Either[E, A] =
      a.asRight[E]

    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
      fa match {
        case Left(e) => Left(e)
        case Right(a) => f(a)
      }

    override def tailRecM[A, B](a: A)(f: A => Either[E, Either[A, B]]): Either[E, B] =
      ???
  }

  def main(args: Array[String]): Unit = {
    val a = 5.asRight[String] // Right(5): Either[String, Int]
    val x = a.flatMap(i => (i + 1).asRight[String])
    val y = 5.asRight[String].flatMap(_ => "boom".asLeft[Int]).flatMap(i => (i + 1).asRight[String])
    println(x)
    println(y)
  }
}
