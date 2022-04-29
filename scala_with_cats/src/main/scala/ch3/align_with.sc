import cats._
import cats.data._
import cats.implicits._

val l1 = List(1, 2, 3)
val l2 = List("1", "2", "3", "4")
l1.alignWith(l2) {
  case Ior.Both(a, b) => a.toString == b
  case Ior.Left(a) => a
  case Ior.Right(b) => b
}
