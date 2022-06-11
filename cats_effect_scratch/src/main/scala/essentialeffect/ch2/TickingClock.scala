package essentialeffect.ch2

import cats.effect._

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

object TickingClock extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = tickingClock.as(ExitCode.Success)
  val tickingClock: IO[Unit] = for {
    _ <- IO(println(System.currentTimeMillis))
    _ <- IO.sleep(FiniteDuration.apply(1, TimeUnit.SECONDS))
    _ <- tickingClock
  } yield ()
}
