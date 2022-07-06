package essentialeffect.ch5

import cats.effect.{ExitCode, IO, IOApp}
import essentialeffect.DebugHelper

import scala.concurrent.duration._

object CedeExample extends IOApp{
  override def run(args: List[String]): IO[ExitCode] = loop().as(ExitCode.Success)

  // Cats effect 3 will auto shift thread scheduling
  def loop(): IO[Unit] = {
    for {
      _ <- IO(println("Hello world")).debug
      _ <- IO.sleep(1.second).debug
//      _ <- IO.cede.debug
      _ <- loop()
    } yield ()
//    IO(println("Hello world")).debug
//      .flatMap(_ => IO.sleep(1.seconds)).debug
//      .flatMap(_ => IO.cede.debug)
//      .flatMap(_ => loop()).debug
  }
}
