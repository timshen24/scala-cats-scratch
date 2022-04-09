import cats._
import cats.implicits._

case class Account(id: Long, number: String, balance: Double, owner: String)

object Account {
  implicit val orderById: Order[Account] =
    Order.from((a1, a2) => Order[Long].compare(a1.id, a2.id) match {
      case 0 => Order[String].compare(a1.number, a2.number)
      case v if v > 0 => 1
      case _ => -1
    })

  object Instances {
    implicit val orderByNumber: Order[Account] = Order.by(_.number)
    // provide an instances of Order[Account] that orders by balance
    implicit def orderByBalance(implicit orderDouble: Order[Double]): Order[Account] =
      Order.by(_.balance)
  }
}

def sort[A](list: List[A])(implicit orderA: Order[A]): List[A] = {
  list.sorted(orderA.toOrdering)
}

val account1 = Account(1, "442-21", 3000, "Julia")
val account2 = Account(2, "442-21", 2500, "Romeo")
//import Account.Instances.orderByBalance
sort[Account](List(account1, account2))
account1 min account2

implicit val orderByIdDesc: Order[Account] = Order.reverse(Account.orderById)
sort[Account](List(account1, account2))