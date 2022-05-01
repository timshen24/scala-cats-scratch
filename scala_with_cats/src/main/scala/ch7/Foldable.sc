def show[A](list: List[A]): String =
  list.foldLeft("nil")((accum, item) => s"$item then $accum")

show(Nil)
show(List(1, 2, 3))

List(1, 2, 3).foldLeft(0)(_ - _) // -6
List(1, 2, 3).foldRight(0)(_ - _) // 2

List(1, 2, 3).foldLeft(List.empty[Int])((acc, i) => i :: acc) // List(3, 2, 1)
List(1, 2, 3).foldRight(List.empty[Int])((i, acc) => i :: acc) // List(1, 2, 3)

def map[A, B](list: List[A])(func: A => B): List[B] = {
  list.foldRight(List.empty[B]) { (item, accum) =>
    func(item) :: accum
  }
}
map(List(1, 2, 3))(_ * 2)

def flatMap[A, B](list: List[A])(func: A => List[B]): List[B] =
  list.foldRight(List.empty[B]) {
    (item, accum) => func(item) ::: accum
  }
flatMap(List(1, 2, 3))(a => List(a, a * 10, a * 100))

def filter[A](list: List[A])(func: A => Boolean): List[A] =
  list.foldRight(List.empty[A]) {
    (item, accum) => if (func(item)) item :: accum else accum
  }
filter(List(1, 2, 3))(_ % 2 == 1)

def sumWithNumeric[A](list: List[A])(implicit numeric: Numeric[A]): A =
  list.foldRight(numeric.zero)(numeric.plus)

sumWithNumeric(List(1, 2, 3))

import cats.Monoid

def sumWithMonoid[A](list: List[A])(implicit monoid: Monoid[A]): A =
  list.foldRight(monoid.empty)(monoid.combine)

import cats.instances.int._

sumWithMonoid(List(1, 2, 3))

import cats.Foldable
import cats.instances.list._

val ints = List(1, 2, 3)
Foldable[List].foldLeft(ints, 0)(_ + _)

import cats.instances.option._

val maybeInt = Option(123)
Foldable[Option].foldLeft(maybeInt, 10)(_ * _)

import cats.Eval
import cats.instances.lazyList._

def bigData = (1 to 100000).to(LazyList)

val eval: Eval[Long] =
  Foldable[LazyList].foldRight(bigData, Eval.now(0L)) {
    (num, eval) => eval.map(_ + num)
  }
eval.value

Foldable[Option].nonEmpty(Option(42))
Foldable[List].find(List(1, 2, 3))(_ % 2 == 0)

Foldable[List].combineAll(List(1, 2, 3))

import cats.instances.string._
Foldable[List].foldMap(List(1, 2, 3))(_.toString)

import cats.instances.vector._ // for Monoid
val ints = List(Vector(1, 2, 3), Vector(4, 5, 6))
(Foldable[List] compose Foldable[Vector]).combineAll(ints)

import cats.syntax.foldable._
List(1, 2, 3).combineAll
List(1, 2, 3).foldMap(_.toString)

