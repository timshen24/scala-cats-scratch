import scala.annotation.tailrec

@tailrec
def fact(n: Int, acc: Int = 1): Int =
  if (n == 0) {
    acc
  } else {
    fact(n - 1, n * acc)
  }

fact(5)

object X_blowup {
  def isEven(n: Int): Boolean =
    if (n == 0) true
    else isOdd(n - 1)

  def isOdd(n: Int): Boolean =
    if (n == 1) true
    else isEven(n - 1)
}

// isEven(10000000)  Blow up

trait Trampoline[+A]

object Trampoline {
  case class Done[A](a: A)                   extends Trampoline[A]
  case class More[A](f: () => Trampoline[A]) extends Trampoline[A]
  case class FlatMap[A, B](ta: Trampoline[A], f: A => Trampoline[B]) extends Trampoline[B]

  def resume[A](ta: Trampoline[A]): Either[() => Trampoline[A], A] = ta match {
    case Done(a) => Right(a)
    case FlatMap(ta, f) => ta match {
      case Done(a) => resume(f(a))
      case FlatMap(ta, f2) => resume(FlatMap(ta, (x: Any) => FlatMap(f2(x), f)))
      case More(thunk2) => Left(() => FlatMap(thunk2(), f))
    }
    case More(f) => resume(f())
  }

  @tailrec
  def run[A](ta: Trampoline[A]): A = resume(ta) match {
    case Left(thunk) => run(thunk())
    case Right(a) => a
  }
}

import Trampoline._
object X {
  def isEven(n: Int): Trampoline[Boolean] =
    if (n == 0) Done(true)
    else More(() => isOdd(n - 1))

  def isOdd(n: Int): Trampoline[Boolean] =
    if (n == 0) Done(false)
    else More(() => isEven(n - 1))
}

import X._
run(isEven(10001))

def flatMap[A, B](as: List[A])(f: A => List[B]): Trampoline[List[B]] =
  as match {
    case head :: tail =>
      More { () =>
        FlatMap(flatMap(tail)(f), (lb: List[B]) => Done(f(head) ::: lb))
      }
    case Nil => Done(Nil)
  }

run(flatMap((1 to 10000).toList)(i => List(i, i + 1)))
