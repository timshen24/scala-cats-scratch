import cats.Functor
import cats.instances.list._
import cats.instances.option._
import cats.implicits._

val list1 = List(1, 2, 3)
Functor[List].map(list1)(_ * 2)

Functor[Option].map(Option(123))(_.toString)

val func       = (x: Int) => x + 1
val liftedFunc = Functor[Option].lift(func)
liftedFunc(Option(1))

Functor[List].as(list1, "As")

def doMath[F[_]: Functor](start: F[Int]): F[Int] =
  start.map(n => n + 1 * 2)

doMath(Option(20))
doMath(List(1, 2, 3))

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

val future: Future[String] =
  Future(123)
    .map(n => n + 1)
    .map(_ * 2)
    .map(n => s"$n!")
println(Await.result(future, 1.second))

sealed trait Tree[A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A)                        extends Tree[A]

object Tree {
  implicit val treeFunctor: Functor[Tree] =
    new Functor[Tree] {
      override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
        fa match {
          case Branch(left, right) => Branch(map(left)(f), map(right)(f))
          case Leaf(value) => Leaf(f(value))
        }
    }

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)
}

import Tree._
Tree.branch(Leaf(10), Leaf(20)).map(_ * 2)
Tree.leaf(100).map(_ * 2)