import cats.data.Reader

final case class Cat(name: String, favoriteFood: String)

val catName: Reader[Cat, String] = Reader(cat => cat.name)

catName.run(Cat("Garfield", "lasagne"))

val greetKitty: Reader[Cat, String] =
  catName.map(name => s"Hello $name")

greetKitty.run(Cat("Healthcliff", "junk food"))

val feedKitty: Reader[Cat, String] =
  Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

val greetAndFeed: Reader[Cat, String] =
  for {
    greet <- greetKitty
    feed <- feedKitty
  } yield s"$greet. $feed"

greetAndFeed(Cat("Garfield", "lasagne"))
greetAndFeed(Cat("Healthcliff", "junk food"))