import cats._
import cats.implicits._

def factorial(n: BigInt): Eval[BigInt] = {
  if (n == 1) {
    Eval.now(n)
  } else {
    Eval.defer(factorial(n - 1).map(_ * n))
  }
}

factorial(10000).value

def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
  as match {
    case head :: tail => Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
    case Nil => acc
  }

def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
  foldRightEval(as, Eval.now(acc))((a, b) => b.map(fn(a, _))).value

foldRight((1 to 100000).toList, 0L)(_ + _)