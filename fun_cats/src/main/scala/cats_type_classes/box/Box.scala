package cats_type_classes.box

import cats._
import cats.implicits._

case class Box[A](a: A)

object Box {
  // implement an instance of Eq[Box[A]] for any A that has an Eq instance
  implicit def eqBox[A](implicit eqA: Eq[A]): Eq[Box[A]] = Eq.by(_.a)
  // implement an instance of Monad[Box]
  implicit val monadBox: Monad[Box] = new Monad[Box] {
    override def pure[A](a: A): Box[A] = Box(a)

    override def flatMap[A, B](fa: Box[A])(f: A => Box[B]): Box[B] =
      f(fa.a)

    override def tailRecM[A, B](a: A)(f: A => Box[Either[A, B]]): Box[B] =
      f(a).a match {
        case Left(value) => tailRecM(value)(f)
        case Right(value) => Box(value)
      }
  }
}
