import cats.data._

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

case class Person(id: Long, name: String, emailAddress: String)
case class Account(id: Long, ownerId: Long)

trait AccountRepository {
  val accountRepository: Service
  trait Service {
    def findAccountById(id: Long): Account
    def saveAccount(account: Account): Unit
  }
}

trait LiveAccountRepository extends AccountRepository {
  override val accountRepository = new Service {
    def findAccountById(id: Long): Account = Account(id, 2)

    override def saveAccount(account: Account): Unit =
      println(s"Account: $account saved.")
  }
}

trait PersonRepository {
  val personRepository: Service
  trait Service {
    def findPersonById(id: Long): Person
  }
}

trait LivePersonRepository extends PersonRepository {
  override val personRepository = (id: Long) => Person(2, "Leandro", "Leandro@163.com")
}

trait EmailRepository {
  val emailRepository: Service

  trait Service {
    def sendEmail(address: String, text: String): Unit
  }
}

trait LiveEmailRepository extends EmailRepository {
  val emailRepository = (address: String, text: String) => println(s"Email sent to $address, text = $text")
}

def findNextAccount(id: Long): Reader[AccountRepository, Account] =
  for {
    accountRepository <- Reader(identity[AccountRepository])
    account = accountRepository.accountRepository.findAccountById(id + 1)
  } yield account

type Env = PersonRepository with AccountRepository with EmailRepository

def findOwnerNameByAccountId(id: Long): Reader[Env, String] =
  for {
    accountModule <- Reader(identity[AccountRepository])
    personModule <- Reader(identity[PersonRepository])
    account = accountModule.accountRepository.findAccountById(id)
    owner = personModule.personRepository.findPersonById(account.ownerId)
  } yield owner.name

val liveEnv: Env = new LivePersonRepository with LiveAccountRepository with LiveEmailRepository
//val fakeEnv: Env = new FakePersonRepository with FakeAccountRepository

findOwnerNameByAccountId(id = 1).run(liveEnv)

def openAccount(accountId: Int, ownerId: Int): Reader[Env, Account] = {
  for {
    accountModule <- Reader(identity[AccountRepository])
    personModule <- Reader(identity[PersonRepository])
    emailModule <- Reader(identity[EmailRepository])
    account = Account(accountId, ownerId)
    _ = accountModule.accountRepository.saveAccount(account)
    person = personModule.personRepository.findPersonById(account.ownerId)
    _ = emailModule.emailRepository.sendEmail(person.emailAddress, "hello world")
  } yield account
}

openAccount(3, 3).run(liveEnv)