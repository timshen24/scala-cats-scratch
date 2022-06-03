package essentialeffect.ch1

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

object Timing extends App {
  val clock: MyIO[Long] = MyIO(() => System.currentTimeMillis)

  def time[A](action: MyIO[A]): MyIO[(FiniteDuration, A)] =
    for {
      t1 <- clock
      act <- action
      t2 <- clock
    } yield (FiniteDuration(t2 - t1, TimeUnit.MILLISECONDS), act)

  val timedHello: MyIO[(FiniteDuration, Unit)] = Timing.time(MyIO.putStr("hello"))

  timedHello.unsafe() match {
    case (duration, _) => println(s"'hello' took $duration")
  } }

