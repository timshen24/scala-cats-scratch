package essentialeffect.ch4

import cats.effect.{ExitCode, IO, IOApp}
import essentialeffect.DebugHelper

import scala.concurrent.duration._

object Timeout extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- task.timeout(500.millis)
    } yield ExitCode.Success

  val task: IO[Unit] = annotatedSleep("   task", 1000.millis)

  def annotatedSleep(name: String, duration: FiniteDuration): IO[Unit] =
    (
      IO(s"$name: starting").debug >>
        IO.sleep(duration) >>
        IO(s"$name: done").debug
    ).onCancel(IO(s"$name: cancelled").debug.void).void
}
