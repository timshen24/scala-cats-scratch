import cats._
import cats.implicits._

Monoid[String].combine("Hi ", "there")
Semigroup[String].combine("Hi ", "there")

Monoid[String].empty
val a = Option(22)
val b = Option(20)
println(Monoid[Option[Int]].combine(a, b))

val stringResult = "Hi " |+| "there" |+| Monoid[String].empty
println(stringResult)
println(1 |+| 2 |+| Monoid[Int].empty)

case class Order(totalCost: Double, quantity: Double)
implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
  override def empty = Order(0, 0)

  override def combine(x: Order, y: Order) =
    Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
}

def add[A: Monoid](items: List[A]): A =
  items.foldLeft(Monoid[A].empty)(_ |+| _)

add(List(Some(1), None, Some(2), None, Some(3)))