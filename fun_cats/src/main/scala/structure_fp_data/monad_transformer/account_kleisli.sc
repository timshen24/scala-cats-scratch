import cats._
import cats.data._
import cats.implicits._

case class Account (id: Long, balance: Double) {
  def updateBalance(f: Double => Double): Account =
    copy(balance = f(balance))
}

trait AccountRepo
type ErrorOr[A] = Either[String, A]
type AccountOp[A] = ReaderT[ErrorOr, AccountRepo, A]

def findAccountById(id: Long): AccountOp[Account] = ???
def saveAccount(account: Account): AccountOp[Account] = ???

def depositMoney(accountId: Long, amount: Double): AccountOp[Account] = {
  for {
    account <- findAccountById(accountId)
    savedAccount <- saveAccount(account.updateBalance(_ + amount))
  } yield savedAccount
}