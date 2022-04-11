import scala.annotation.tailrec

trait MList[+A]

case class MCons[+A](hd: A, tl: MList[A]) extends MList[A]
case object MNil extends MList[Nothing]

//def sum(ints: MList[Int]): Int =
//  ints match {
//    case MCons(hd, tl) => hd + sum(tl)
//    case MNil => 0
//  }
//
//def length[A](list: MList[A]): Int =
//  list match {
//    case MCons(_, tl) => 1 + length(tl)
//    case MNil => 0
//  }
//
//def filterPositive(ints: MList[Int]): MList[Int] =
//  ints match {
//    case MCons(hd, tl) => if (hd > 0) MCons(hd, filterPositive(tl)) else
//      filterPositive(ints)
//    case MNil => MNil
//  }

def foldRight[A, B](list: MList[A])(z: B)(f: (A, B) => B): B =
  list match {
    case MCons(hd, tl) => f(hd, foldRight(tl)(z)(f))
    case MNil => z
  }

def sum(ints: MList[Int]): Int =
  foldRight(ints)(0)(_ + _)

def length[A](list: MList[A]): Int =
  foldRight(list)(0)((_, y) => 1 + y)

def filterPositive(ints: MList[Int]): MList[Int] =
  foldRight(ints)(MNil)((x: Int, y: MList[Int]) => if (x > 0) MCons(x, y) else y)
