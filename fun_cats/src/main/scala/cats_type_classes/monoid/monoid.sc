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

val sumMonoid: Monoid[Int] = Monoid.instance(0, _ + _)
val minMonoid: Monoid[Int] = Monoid.instance(Int.MaxValue, _ min _)
def listMonoid[A]: Monoid[List[A]] = Monoid.instance(Nil, _ ++ _)
val stringMonoid: Monoid[String] = new Monoid[String] {
  override def empty = ""

  override def combine(x: String, y: String) = x + y
}

sumMonoid.combine(3, 4)
minMonoid.combine(6, 2)
listMonoid[Boolean].combine(List(true, false), List(false, true))
stringMonoid.combine("hello", "world")