import cats._
import cats.implicits._

case class Speed(metersPerSecond: Double) {
  def kilometersPerSec: Double = metersPerSecond / 1000.0
  def milesPerSec: Double = metersPerSecond / 1609.34
}

object Speed {
  def addSpeed(s1: Speed, s2: Speed): Speed =
    Speed(s1.metersPerSecond + s2.metersPerSecond)

//  implicit val monoidSpeed: Monoid[Speed] = new Monoid[Speed] {
//    override def empty = Speed(0)
//
//    override def combine(x: Speed, y: Speed) = Speed(x.metersPerSecond + y.metersPerSecond)
//  }

  implicit val speedMonoid: Monoid[Speed] =
    Monoid.instance[Speed](Speed(0), addSpeed)

  implicit val eqSpeed: Eq[Speed] = Eq.fromUniversalEquals
}

val speed1 = Speed(5)
val speed2 = Speed(7)
Speed.speedMonoid.combine(speed1, speed2)
Monoid[Speed].empty

speed1 |+| speed2
val list = List(Speed(100), Speed(200), Speed(3000))
Monoid[Speed].combineAll(list)
list.combineAll
Monoid.isEmpty(Speed(100)) // because I have implicit value eqSpeed
Monoid.isEmpty(Speed(0))

