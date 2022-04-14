import cats.data._
import cats._
import cats.implicits._

val signReader: Reader[Int, String] // Int => String
 = Reader(n => if (n > 0) "positive" else if (n < 0) "negative" else "zero")

signReader.run(5)

val parityReader: Reader[Int, String] =
  Reader(n => if (n % 2 == 1) "odd" else "even")

parityReader.run(4)

val descriptionReader: Reader[Int, String] =
  for {
    sign <- signReader
    parity <- parityReader
  } yield s"$sign and $parity"
descriptionReader(5)
descriptionReader(-6)

val addOneReader: Reader[Int, Int] =
  for {
    env <- Reader(/*(x: Int) => x*/identity[Int]) // ask
  } yield env + 1

addOneReader.run(1)

case class Person(id: Long, name: String)
case class Account(id: Long, ownerId: Long)

trait AccountRepository {
  val accountRepository: Service
  trait Service {
    def findAccountById(id: Long): Account
  }
}

trait LiveAccountRepository extends AccountRepository {
  override val accountRepository = (id: Long) => Account(id, 2)
}

trait PersonRepository {
  val personRepository: Service
  trait Service {
    def findPersonById(id: Long): Person
  }
}

trait LivePersonRepository extends PersonRepository {
  override val personRepository = (id: Long) => Person(2, "Leandro")
}

def findNextAccount(id: Long): Reader[AccountRepository, Account] =
  for {
    accountRepository <- Reader(identity[AccountRepository])
    account = accountRepository.accountRepository.findAccountById(id + 1)
  } yield account

def findOwnerNameByAccountId(id: Long): Reader[Env, String] =
  for {
    accountModule <- Reader(identity[AccountRepository])
    personModule <- Reader(identity[PersonRepository])
    account = accountModule.accountRepository.findAccountById(id)
    owner = personModule.personRepository.findPersonById(account.ownerId)
  } yield owner.name

type Env = PersonRepository with AccountRepository
val liveEnv: Env = new LivePersonRepository with LiveAccountRepository
//val fakeEnv: Env = new FakePersonRepository with FakeAccountRepository

findOwnerNameByAccountId(id = 1).run(liveEnv)