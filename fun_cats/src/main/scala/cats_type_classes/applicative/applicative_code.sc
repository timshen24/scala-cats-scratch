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

val optionApplicative: Applicative[Option] = new Applicative[Option] {
  override def pure[A](a: A): Option[A] = Option(a)

  override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] =
    (ff, fa) match {
      case (Some(f), Some(a)) => Some(f(a))
      case _ => None
    }
}

val o1 = optionApplicative.pure(1)
val o2 = optionApplicative.pure(null)
(o1, o2).mapN(_ + _)

val listApplicative: Applicative[List] = new Applicative[List] {
  override def pure[A](a: A): List[A] = List(a)

  override def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] =
    (ff, fa) match {
      case (f :: fs, a :: as) => (a :: as).fmap(f) ++ ap(fs)(a :: as)
      case _ => Nil
    }
}

val l1 = listApplicative.pure(List[Int](1, 2, 3))
val l2 = listApplicative.pure(List[Int](4, 5))
listApplicative.map2(List(1, 2, 3), List(4, 5))(_ + _)
listApplicative.map2[Int, Int, Int](List(1, 2, 3), List())(_ + _)

val l3 = List(1, 2, 3)
val l4 = List(4, 5)
(l3, l4).mapN(_ + _)
(l3, l4).parMapN(_ + _)