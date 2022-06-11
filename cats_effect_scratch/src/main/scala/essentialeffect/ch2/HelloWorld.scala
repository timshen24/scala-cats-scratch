package essentialeffect.ch2

import cats.effect.{ExitCode, IO, IOApp}

object HelloWorld extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = helloWorld.as(ExitCode.Success)
  val helloWorld: IO[Unit] = IO(println("Hello World!"))
}
