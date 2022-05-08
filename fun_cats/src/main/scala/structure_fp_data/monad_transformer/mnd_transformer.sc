import cats.data._
import cats.implicits._

trait AccountRepo
type ErrorOr[A] = Either[String, A]
// ReaderT is just Kleisli
type AccountOp[A] = ReaderT[ErrorOr, AccountRepo, A]

val dummyRepo: AccountRepo = new AccountRepo {}

// ErrorOr[B] -> ReaderT[ErrorOr, A, B]
ReaderT.liftF[ErrorOr, AccountRepo, Int](5.asRight[String]).run(dummyRepo)
ReaderT.liftF[ErrorOr, AccountRepo, Int]("hello".asLeft[Int]).run(dummyRepo)

// B -> ReaderT[ErrorOr, A, B]
5.pure[AccountOp].run(dummyRepo)

// ReaderT[ErrorOr, AccountOp, B] -> ReaderT[Option, AccountOp, B]
// change context of Kleisli
val mapF = 5.pure[AccountOp].mapF {
  case Left(_) => None
  case Right(value) => Some(value)
}
mapF.run(dummyRepo)

// Apply method for Kleisli
ReaderT((_: AccountRepo)=> 5.asRight[String]).run(dummyRepo)
ReaderT((_: AccountRepo)=> "hello".asLeft[Int]).run(dummyRepo)

5.pure[AccountOp].map(_ + 1).run(dummyRepo)
ReaderT((_: AccountRepo) => "hello".asLeft[Int]).map(_ + 1).run(dummyRepo)

5.pure[AccountOp].flatMap(i => (i + 1).pure[AccountOp]).run(dummyRepo)
ReaderT((_: AccountRepo) => "hello".asLeft[Int])
  .flatMap(i => (i + 1).pure[AccountOp])
  .run(dummyRepo)


