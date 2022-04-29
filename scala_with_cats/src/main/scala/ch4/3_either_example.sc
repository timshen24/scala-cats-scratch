import cats.syntax.either._

def countPositive(nums: List[Int]): Either[String, Int] =
  nums.foldLeft(0.asRight[String]){(acc, num) =>
    if (num >= 0)
//      acc.flatMap(i => (i + 1).asRight[String])
      acc.map(_ + 1)
    else
//      "Must be positive".asLeft[Int]
      Left("Must be positive")
  }

countPositive(List(1, 2, 3))
countPositive(List(1, 2, -3))

6.asRight[String].bimap(_.reverse, _ * 7)
"bar".asLeft[Int].bimap(_.reverse, _ * 7)
