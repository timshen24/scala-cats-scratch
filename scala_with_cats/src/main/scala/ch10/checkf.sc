import cats._
import cats.implicits._

final case class CheckF[E: Semigroup, A](func: A => Either[E, A]) {
  def apply(a: A): Either[E, A] = func(a)

  def and(that: CheckF[E, A]): CheckF[E, A] =
    CheckF {
      a => (this.func(a), that.func(a)) match {
        case (Left(e1), Left(e2)) => Left(e1 |+| e2)
        case (Left(e1), Right(_)) => Left(e1)
        case (Right(_), Left(e2)) => Left(e2)
        case (Right(_), Right(_)) => Right(a)
      }
    }
}

val a: CheckF[List[String], Int] =
  CheckF { v =>
    if (v > 2) v.asRight
    else List("Must be > 2").asLeft
  }

val b: CheckF[List[String], Int] =
  CheckF { v =>
    if (v < -2) v.asRight
    else List("Must be < -2").asLeft
  }

val ab: CheckF[List[String], Int] = a and b
ab(2)