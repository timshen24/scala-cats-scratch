package essentialeffect.ch3

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import essentialeffect.DebugHelper

import scala.concurrent.duration._

object MyParMapNErrors extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = res.as(ExitCode.Success)

  val ok: IO[String] = IO("hi").debug
  val ko1: IO[String] = IO.raiseError[String](new RuntimeException("oh!")).debug
  val ko2: IO[String] = IO.raiseError[String](new RuntimeException("noes!")).debug

  val e1: IO[Unit] = (ok, ko1).parMapN((_, _) => ())
  val e2: IO[Unit] = (ko1, ok).parMapN((_, _) => ())
  val e3: IO[Unit] = (ko1, ko2).parMapN((_, _) => ())

  // if no attempt, the program will boom with RuntimeException
  val repeat: IO[Unit] = IO("Starting new round:").debug >> e1.attempt.debug >>
    IO("---").debug >>
    e2.attempt.debug >>
    IO("---").debug >>
    e3.attempt.debug >> IO("Round ends.\n").debug >> IO.pure(())
//    IO.pure(ExitCode.Success)

  val res: IO[Unit] = for {
    _ <- repeat
    _ <- IO.sleep(3.seconds)
    _ <- res
  } yield ()
}
