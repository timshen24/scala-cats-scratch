import scala.annotation.tailrec

@tailrec
def fact(n: Int, acc: Int = 1): Int = {
  if (n == 0) {
    acc
  } else {
    fact(n - 1, n * acc)
  }
}

fact(5)