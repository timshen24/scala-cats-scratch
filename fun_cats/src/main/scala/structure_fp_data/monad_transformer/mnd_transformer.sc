import cats._
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



// begin OptionT
type ErrorOrOpt[A] = OptionT[ErrorOr, A] // this is just ErrorOr[Option[A]]
type AccountOp2[A] = ReaderT[ErrorOrOpt, AccountRepo, A]

5.pure[AccountOp2].run(dummyRepo) // AccountRepo => Either[String, Option[A]]
5.pure[AccountOp2].flatMap(n => (n + 1).pure[AccountOp2]).run(dummyRepo)
5.pure[ErrorOrOpt].flatMap(n => (n + 1).pure[ErrorOrOpt])
Option(5).asRight[String] // ErrorOr[Int]
OptionT(Option(5).asRight[String]) // OptionT[ErrorOr, Int]
OptionT(Option.empty[Int].asRight[String]).flatMap(n => (n + 1).pure[ErrorOrOpt])
"boom".asLeft[Option[Int]].flatMap(n => Right(n.map(_ + 1)))
OptionT("boom".asLeft[Option[Int]]).flatMap(n => (n + 1).pure[ErrorOrOpt])

// operate on Option
OptionT(Option(5).asRight[String]).subflatMap(n => Option(n + 1))
OptionT("boom".asLeft[Option[Int]]).subflatMap(n => Option(n + 1))
OptionT(Option.empty[Int].asRight[String]).subflatMap(n => Option(n + 1))

// operate on Either
OptionT(Option(5).asRight[String]).semiflatMap(n => Right(n + 1))
OptionT("boom".asLeft[Option[Int]]).semiflatMap(n => Right(n + 1))
OptionT(Option.empty[Int].asRight[String]).semiflatMap(n => Right(n + 1))

OptionT(Option(5).asRight[String]).flatMapF(n => Right(Some(n + 1)))
