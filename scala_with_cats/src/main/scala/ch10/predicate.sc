import cats._
import cats.data.Validated._
import cats.data._
import cats.implicits._

sealed trait Predicate[E, A] {
  def and(that: Predicate[E, A]): Predicate[E, A] =
    And(this, that)

  def or(that: Predicate[E, A]): Predicate[E, A] =
    Or(this, that)

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
    this match {
      case And(left, right) => (left(a), right(a)).mapN((_, _) => a)
      case Or(left, right) => left(a) match {
        case Valid(_) => Valid(a)
        case Invalid(e1) => right(a) match {
          case Valid(_) => Valid(a)
          case Invalid(e2) => Invalid(e1 |+| e2)
        }
      }
      case Pure(func) => func(a)
    }

}

final case class And[E, A] (left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

final case class Or[E, A] (left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

final case class Pure[E, A](func: A => Validated[E, A]) extends Predicate[E, A]

sealed trait Check[E, A, B] {
  import Check._

  def apply(in: A)(implicit s: Semigroup[E]): Validated[E, B]

  def map[C](f: B => C): Check[E, A, C] =
    Map[E, A, B, C](check = this, f)
}

object Check {
  final case class Map[E, A, B, C](check: Check[E, A, B], func: B => C) extends Check[E, A, C] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(in).map(func)
  }

  final case class Pure[E, A](pred: Predicate[E, A]) extends Check[E, A, A] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, A] = pred(in)
  }

  def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] = {
    Pure(pred)
  }
}