import cats._
import cats.data._
import cats.implicits._

case class Client(id: Long, name: String, age: Int)
case class Product(id: Long, name: String, unitPrice: Double)
case class ShoppingCartItem(quantity: Int, product: Product) {
  def total: Double = quantity * product.unitPrice
}
case class ShoppingCart(client: Client, items: List[ShoppingCartItem]) {
  def total: Double = items.map(_.total).sum
}

trait Discount {
  val name: String
  def applies(client: Client, item: ShoppingCartItem): Boolean
  def amountToDiscount(item: ShoppingCartItem): Double
  def calculateDiscountForClient(client: Client, item: ShoppingCartItem): Double = {
    if (applies(client, item))
      amountToDiscount(item)
    else 0
  }
}

object moreThanFiveUnitsDiscount extends Discount {
  override val name = "10% discount on more than 5 units"

  override def applies(client: Client, item: ShoppingCartItem) =
    item.quantity > 5

  override def amountToDiscount(item: ShoppingCartItem) =
    item.total * 0.1
}

object ElderlyDiscount extends Discount {
  override val name = "20% discount for people older than 65"

  override def applies(client: Client, item: ShoppingCartItem) =
    client.age >= 65

  override def amountToDiscount(item: ShoppingCartItem) =
    item.total * 0.2
}

// This one is awesome
def calculateTotalDiscount(cart: ShoppingCart, discounts: List[Discount]): Double = {
  (cart.items, discounts).mapN {(item, discount) =>
    discount.calculateDiscountForClient(cart.client, item)
  }.sum
}

val l1 = List(1, 2, 3)
val l2 = List("1", "2", "3")
(l1, l2).mapN {(id, value) => id == value.toInt}

