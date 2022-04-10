import cats._
import cats.implicits._

val listMonad: Monad[List] = new Monad[List] {
  override def pure[A](x: A) = List(x)

  override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
    fa match {
      case a :: as => f(a) ++ flatMap(as)(f)
      case Nil => Nil
    }

  override def tailRecM[A, B](a: A)(f: A => List[Either[A, B]]) = ???
}

listMonad.flatMap(List(1, 2, 3))(a => List(a + 1, a + 2))