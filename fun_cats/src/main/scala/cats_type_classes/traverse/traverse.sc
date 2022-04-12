import cats._
import cats.implicits._
import cats.data._

import scala.annotation.tailrec

trait MList[+A]

object MList {
  case class MCons[+A](hd: A, tl: MList[A]) extends MList[A]
  case object MNil                          extends MList[Nothing]

  def apply[A](elems: A*): MList[A] =
    elems.foldRight(mnil[A])((a, b) => mcons(a, b))

  def mnil[A]: MList[A] = MNil

  def mcons[A](hd: A, tl: MList[A]): MList[A] = MCons(hd, tl)

  implicit val mlistFunctor: Functor[MList] = new Functor[MList] {
    override def map[A, B](fa: MList[A])(f: A => B): MList[B] = fa match {
      case MCons(hd, tl) => MCons(f(hd), map(tl)(f))
      case MNil => MNil
    }
  }

  // this version of traverse should be implemented by self
  implicit val listTraverse: Traverse[MList] = new Traverse[MList] {
    override def traverse[G[_]: Applicative, A, B](fa: MList[A])(f: A => G[B]): G[MList[B]] =
      fa match {
        case MCons(h, t) => (f(h), traverse(t)(f)).mapN(MCons.apply)
        case MNil => Applicative[G].pure(MNil)
      }
//      sequence(fa.map(f))

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

    override def sequence[G[_]: Applicative, A](fga: MList[G[A]]): G[MList[A]] =
      traverse(fga)(identity)
  }
}

Traverse[MList].sequence(MList(Option(5), Option(4)))
Traverse[MList].sequence(MList(Option(5), None))
Traverse[MList].traverse(MList(1, 2, 3))(i => Option(i + 1))