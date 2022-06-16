import cats._
import cats.implicits._

import scala.annotation.tailrec

trait MList[+A]

object MList {
  case class MCons[+A](hd: A, tl: MList[A]) extends MList[A]
  case object MNil                          extends MList[Nothing]

  def apply[A](elems: A*): MList[A] =
    elems.foldRight(mnil[A])((a, b) => mcons(a, b))

  def mnil[A]: MList[A] = MNil

  def mcons[A](hd: A, tl: MList[A]): MList[A] = MCons(hd, tl)

  implicit val listFoldable: Foldable[MList] = new Foldable[MList] {
    @tailrec
    override def foldLeft[A, B](fa: MList[A], b: B)(f: (B, A) => B): B = fa match {
      case MCons(h, t) => foldLeft(t, f(b, h))(f)
      case MNil        => b
    }

    override def foldRight[A, B](fa: MList[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      def loop(as: MList[A]): Eval[B] =
        as match {
          case MCons(h, t) => f(h, Eval.defer(loop(t)))
          case MNil        => lb
        }
      Eval.defer(loop(fa))
    }
  }
}

import MList._
MList(1, 2, 3)

def sum(ints: MList[Int]): Int =
//  ints match {
//    case MCons(hd, tl) => hd + sum(tl)
//    case MNil          => 0
//  }
//  Foldable[MList].foldLeft(ints, 0)(_ + _)
    ints.foldLeft[Int](0)(_ + _)

def length(ints: MList[Int]): Int =
//  ints match {
//    case MCons(_, tl) => 1 + length(tl)
//    case MNil         => 0
//  }
  Foldable[MList].foldLeft(ints, 0)((b, _) => 1 + b)

def filterPositive(ints: MList[Int]): MList[Int] =
//  ints match {
//    case MCons(hd, tl) => if (hd > 0) MCons(hd, filterPositive(tl)) else filterPositive(tl)
//    case MNil          => MNil
//  }
//  Foldable[MList].foldLeft(ints, mnil[Int])((b, a) => if (a > 0) mcons[Int](a, b) else b)
    ints.foldRight(Eval.now(mnil[Int]))((i, evalInts) => if (i > 0) Eval.now(mcons(i, evalInts.value)) else evalInts).value

sum(MList(1, 2, 3))
length(MList(1, 2, 3, 4, 5))
filterPositive(MList(-1, 0, 1, 2))

MList(1, 2, 3).foldMap(_.show)
MList(1, 2, 3).foldMap(_ * 2)
MList(1, 2, 3).fold
MList("hello", "world").fold

def find[F[_]: Foldable, A](fa: F[A])(p: A => Boolean): Option[A] =
  fa.foldLeft[Option[A]](None)((b, a) => if (p(a)) Some(a) else b)

find[MList, Int](MList(1, 2, 3))(i => i % 2 == 0)

def exists[F[_]: Foldable, A](fa: F[A])(p: A => Boolean): Boolean = {
//  find(fa)(p).nonEmpty
  fa.foldLeft[Boolean](false)((b, a) => b || p(a))
}

exists[MList, Int](MList(1, 2, 3))(i => i % 5 == 0)

def toList[F[_]: Foldable, A](fa: F[A]): MList[A] = {
  fa.foldRight[MList[A]](Eval.now(MNil))((a, b) => Eval.now(MCons(a, b.value))).value
}

toList[MList, Int](MList(1, 2, 3, 4))

def forall[F[_]: Foldable, A](fa: F[A])(p: A => Boolean): Boolean =
  fa.foldLeft[Boolean](true)((b, a) => b && p(a))

forall[MList, Int](MList(1, 2, 3))(_ > 1)
forall[MList, Int](MList(1, 2, 3))(_ >= 1)

List(1, 2, 3, 4).foldLeft(List[Int]())((acc, v) => v :: acc)