package essentialeffect.ch1

case class MyIO[A](unsafe: () => A) {
  def map[B](f: A => B): MyIO[B] = MyIO(() => f(unsafe()))
  def flatMap[B](f: A => MyIO[B]): MyIO[B] = MyIO(() => f(unsafe()).unsafe())
}

object MyIO {
  def putStr(s: => String): MyIO[Unit] = MyIO(() => println(s))
}

object Printing extends App {
  val myIO = MyIO.putStr("hello!")
  myIO.unsafe()

  // substitution
  MyIO(() => println("hello!")).unsafe()
  (() => println("hello!"))()
  println("hello!")

  // composition using map and flatMap
  val hello: MyIO[Unit] = MyIO.putStr("hello!")
  val world: MyIO[Unit] = MyIO.putStr("world!")
  val helloWorld: MyIO[Unit] = for {
    _ <- hello
    _ <- world
  } yield ()
  helloWorld.unsafe()
}


