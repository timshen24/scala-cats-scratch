import cats._
import cats.data._
import cats.implicits._

def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
  for {
    x <- a
    y <- b
  } yield x * x + y * y

sumSquare(1.pure[Option], 2.pure[Option])