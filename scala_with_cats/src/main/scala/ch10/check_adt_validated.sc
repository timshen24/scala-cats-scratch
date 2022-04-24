import cats._
import cats.data.Validated._
import cats.data._
import cats.implicits._

sealed trait Check[E, A] {
  import Check._

  def and(that: Check[E, A]): Check[E, A] =
    And(this, that)

  def or(that: Check[E, A]): Check[E, A] =
    Or(this, that)

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
    this match {
      case Pure(func) => func(a)
      case And(left, right) =>
        (left(a), right(a)).mapN((_, _) => a)
      case Or(left, right) =>
        left(a) match {
          case Valid(a) => Valid(a)
          case Invalid(e1) =>
            right(a) match {
              case Valid(a) => Valid(a)
              case Invalid(e2) => Invalid(e1 |+| e2)
            }
        }
    }
}

object Check {
  final case class And[E, A] (left: Check[E, A], right: Check[E, A]) extends Check[E, A]

  final case class Or[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]

  final case class Pure[E, A](func: A => Validated[E, A]) extends Check[E, A]

  def pure[E, A](f: A => Validated[E, A]): Check[E, A] = Pure(f)
}

val a: Check[List[String], Int] =
  Check.pure { v =>
    if (v > 2) v.valid
    else List("Must be > 2").invalid
  }

val b: Check[List[String], Int] =
  Check.pure { v =>
    if (v < -2) v.valid
    else List("Must be < -2").invalid
  }

val ab: Check[List[String], Int] = a and b
ab(2)