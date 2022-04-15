import cats._
import cats.data._
import cats.implicits._

object Tracked {
  type Tracked[A] = Writer[List[String], A]
  implicit def trackedShow[A: Show]: Show[Tracked[A]] = Show.show { ta =>
    val (log: List[String], a: A) = ta.run
    (log ++ List(a.show)).mkString("\n")
  }
}

import Tracked._

case class Client(id: Long, name: String, age: Int)
object Client {
  def makeRaw(id: Long, name: String, age: Int): Tracked[Client] = {
//    List[String]("Creating client").tell.map(_ => Client(id, name, age))
    Client(id, name, age).writer(List[String]("Creating client"))
  }
}

case class Product(id: Long, name: String, unitPrice: Double)
object Product {
  def makeRaw(id: Long, name: String, unitPrice: Double): Tracked[Product] = {
//    Writer(List[String]("Creating product"), Product(id, name, unitPrice))
    Product(id, name, unitPrice).writer(List[String]("Creating product"))
  }
}

case class ShoppingCartItem(quantity: Int, product: Product) {
  def total: Double = quantity * product.unitPrice
}

object ShoppingCartItem {
  implicit val shoppingCartItemShow: Show[ShoppingCartItem] =
    Show.show(item => s"${item.quantity} * ${item.product.name}")

  def makeRaw(
    quantity: Int,
    productId: Long,
    productName: String,
    productUnitPrice: Double
  ): Tracked[ShoppingCartItem] = {
//    Product.makeRaw(productId, productName, productUnitPrice)
//      .flatMap(ShoppingCartItem(quantity, _).writer(List("Creating shopping cart item...")))
    (List("Creating shopping cart item...").tell, Product.makeRaw(productId, productName, productUnitPrice))
      .mapN {
        (_, p) => ShoppingCartItem(quantity, p)
      }
//    for {
//      _ <- List("Creating shopping cart item...").tell
//      product <- Product.makeRaw(productId, productName, productUnitPrice)
//    } yield ShoppingCartItem(quantity, product)
  }
}

case class ShoppingCart(client: Client, items: List[ShoppingCartItem]) {
  def total: Double = items.map(_.total).sum
}

object ShoppingCart {
  implicit val scShow: Show[ShoppingCart] = Show.fromToString[ShoppingCart]

  def makeRaw(
    clientId: Long,
    clientName: String,
    clientAge: Int,
    items: List[(Int, Long, String, Double)]
  ): Tracked[ShoppingCart] = {
    for {
      _ <- List("Creating Shopping Cart...").tell
      client <- Client.makeRaw(clientId, clientName, clientAge)
      scitems <- items.traverse{case(q, pid, pname, pprice) => ShoppingCartItem.makeRaw(q, pid, pname, pprice)}
    } yield ShoppingCart(client, scitems)
  }
}

sealed trait Discount {
  val name: String
  def applies(client: Client, shoppingCartItem: ShoppingCartItem): Boolean
  def getDiscountedAmount(shoppingCartItem: ShoppingCartItem): Double

  def calculateDiscount(client: Client, shoppingCartItem: ShoppingCartItem): Tracked[Double] =
    if (applies(client, shoppingCartItem)) {
      getDiscountedAmount(shoppingCartItem).writer(List(s"Applied discount: $name"))
    } else {
      0d.pure[Tracked]
    }
}

object Discount {
  object MoreThanFiveUnitsDiscount extends Discount {
    override val name = "10% discount on more than 5 units"

    override def applies(client: Client, item: ShoppingCartItem) =
      item.quantity > 5

    override def getDiscountedAmount(item: ShoppingCartItem) =
      item.total * 0.1
  }

  object ElderlyDiscount extends Discount {
    override val name = "20% discount for people older than 65"

    override def applies(client: Client, item: ShoppingCartItem) =
      client.age >= 65

    override def getDiscountedAmount(item: ShoppingCartItem) =
      item.total * 0.2
  }

  val allDiscounts: List[Discount] = List(MoreThanFiveUnitsDiscount, ElderlyDiscount)
}

def calculateTotalDiscount(shoppingCart: ShoppingCart, discounts: List[Discount]): Tracked[Double] =
//  (shoppingCart.items, discounts).mapN { (item, discount) =>
//    discount.calculateDiscount(shoppingCart.client, item)
//  }.combineAll
  (shoppingCart.items, discounts).tupled
//    .foldMap {case (i, d) => d.calculateDiscount(shoppingCart.client, i)}
    .traverse { case (i, d) => d.calculateDiscount(shoppingCart.client, i) }
    .map(_.sum)

def calculateTotal(shoppingCart: ShoppingCart): Tracked[Double] =
  calculateTotalDiscount(shoppingCart, Discount.allDiscounts)
    .map(a => shoppingCart.total - a)

val client = Client(1, "leardro", 70)
val milk   = Product(1, "milk", 15.0)
val eggs   = Product(1, "eggs", 25.0)
val items = List(
  ShoppingCartItem(15, milk),
  ShoppingCartItem(30, eggs)
)
val shoppingCart: ShoppingCart = ShoppingCart(client, items)
Show[Tracked[Double]].show(calculateTotalDiscount(shoppingCart, Discount.allDiscounts))
Show[Tracked[Double]].show(calculateTotal(shoppingCart))

val sc = ShoppingCart.makeRaw(
  1,
  "Leandro",
  70,
  List((1, 3, "eggs", 15), (4, 8, "milk", 30))
)
sc.show
Show[Tracked[ShoppingCart]].show(sc)