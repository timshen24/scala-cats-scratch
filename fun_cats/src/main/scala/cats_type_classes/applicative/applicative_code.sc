import cats._
import cats.implicits._

sealed trait Validated[+A]

object Validated {
  case class Valid[+A](a: A) extends Validated[A]
  case class Invalid(errors: List[String]) extends Validated[Nothing]

  implicit val applicative: Applicative[Validated] = new Applicative[Validated] {
    override def pure[A](a: A): Valid[A] = Valid(a)

    override def ap[A, B](ff: Validated[A => B])(va: Validated[A]): Validated[B] =
      /*(ff, fa) match {
        case (Valid(f), Valid(a)) => Valid(f(a))
        case (Invalid(ex), Valid(a)) => Invalid(ex)
        case (Valid(f), Invalid(a)) => Invalid(a)
        case (Invalid(ex1), Invalid(ex2)) => Invalid(ex1 ++ ex2)
      }*/
      map2(ff, va)((f, a) => f(a))

//    override def map[A, B](va: Validated[A])(f: A => B): Validated[B] = ???

    override def map2[A, B, C](va: Validated[A], vb: Validated[B])(f: (A, B) => C): Validated[C] =
      (va, vb) match {
        case (Valid(a), Valid(b)) => Valid(f(a, b))
        case (Invalid(e), Valid(_)) => Invalid(e)
        case (Valid(_), Invalid(e)) => Invalid(e)
        case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2)
      }
//      ap(ap(pure(f.curried))(va))(vb)

//    override def map3[A, B, C, D](va: Validated[A], vb: Validated[B], vc: Validated[C])(f: (A, B, C) => D): Validated[D] = ???

    def tupled[A, B](va: Validated[A], vb: Validated[B]): Validated[(A, B)] =
      map2(va, vb)((a, b) => (a, b))
  }
}

val v1: Validated[Int] = Applicative[Validated].pure(1)
val v2: Validated[Int] = Applicative[Validated].pure(2)
val v3: Validated[Int] = Applicative[Validated].pure(3)
(v1, v2, v3).mapN(_ + _ + _)
