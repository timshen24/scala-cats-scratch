import cats.data.Kleisli
//  ReaderT[F[_], A, B]  ==
//  ReaderT[Id, A, B]    == //specialising for Id
//  ReaderT(A => Id[B])  ==
//  Reader(A => B)       == //since Id[B] == B
//  Kleisli(A => B)         //since ReaderT == Kleisli

final case class Name(first: String, last: String)

final case class Age(age: Int)

final case class Person(name: Name, age: Age)

final case class Config(name: String, age: Int)


def readName: Config => Option[Name] = c => {
  val parts = c.name.split(" ")
  if (parts.length > 1) Option(Name(parts(0), parts.drop(1).mkString(" "))) else None
}

def readNameK = Kleisli(readName)

def readAge: Config => Option[Age] = c => {
  val age = c.age
  if (age >= 1 && age <= 150) Option(Age(age)) else None
}

def readAgeK = Kleisli(readAge)

import cats.implicits._

val personK: Kleisli[Option, Config, Person] =
  for {
    name <- readNameK
    age  <- readAgeK
  } yield Person(name, age)

//Some(Person(Name(Bojack,Horseman),Age(42)))
val result1 = personK(Config("Bojack Horseman", 42))

//None - Name is not space-separated
val result2 = personK(Config("Jake", 20))

//None - age is not between 1 and 150
val result3 = personK(Config("Fred Flintstone", 50000))

// You might have noticed that the readAgeK function does not directly depend on the output of readNameK.
// This implies that we don’t have to use a Monad here (for-comprehesion)
// and can use something a little less powerful like Apply.
import cats.Apply
import cats.implicits._

type KOptionConfig[A] = Kleisli[Option, Config, A]
type PersonFunc = (Name, Age) => Person

val config = Config("mr peanutbutter", 30)
val readNameKOC: KOptionConfig[Name] = readNameK
val readAgeKOC: KOptionConfig[Age] = readAgeK
//val personKOC: KOptionConfig[PersonFunc] = Kleisli( (_: Config) => Option(Person))

//Kleisli[Option, Config, Person]
//val personK = Apply[KOptionConfig].ap2(personKOC)(readNameKOC, readAgeKOC)
val personK = Apply[KOptionConfig].map2(readNameKOC, readAgeKOC) { Person }

//Some(Person(Name(mr,peanutbutter),Age(30)))
personK(config)

// Now not read from config but from String and Int
def readName: String => Option[Name] = name => {
  val parts = name.split(" ")
  if (parts.length > 1) Option(Name(parts(0), parts.drop(1).mkString(" "))) else None
}

def readAge: Int => Option[Age] = age => {
  if (age >= 1 && age <= 150) Option(Age(age)) else None
}

def readNameK: Kleisli[Option, String, Name] = Kleisli(readName)

def readAgeK: Kleisli[Option, Int, Age] = Kleisli(readAge)

// Config.name -> Kleisli[Option, String, Name] -> Option[Name]
// Config.age -> Kleisli[Option, Int, Age] -> Option[Age]
// Name + Age -> Person
val personK: Kleisli[Option, Config, Person] =
for {
  name <- readNameK.local[Config](_.name)
  age  <- readAgeK.local[Config](_.age)
} yield Person(name, age)

//Some(Person(Name(Bojack,Horseman),Age(42)))
personK(Config("Bojack Horseman", 42))

//None
personK(Config("Jake", 20))

//None
personK(Config("Fred Flintstone", 50000))

val config = Config("Diane Nguyen", 27)
val readNameKOC: KOptionConfig[Name] = readNameK.local[Config](_.name)
val readAgeKOC: KOptionConfig[Age] = readAgeK.local[Config](_.age)

val personK = Apply[KOptionConfig].map2(readNameKOC, readAgeKOC) { Person }

//Some(Person(Name(Diane,Nguyen),Age(27)))