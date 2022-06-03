import cats._
import cats.data._
import cats.implicits._
import cats.effect._
import cats.mtl._
import cats.mtl
import cats.mtl.implicits._

// These are just String for simplicity
type Config = String
type Result = String

def getConfig: IO[Config] = ???
// getConfig: cats.effect.IO[Config]

def serviceCall(c: Config): IO[Result] = ???
// serviceCall: (c: Config)cats.effect.IO[Result]

def readerProgram[F[_]: Monad: LiftIO](implicit A: ApplicativeAsk[F, Config]): F[Result] = for {
  config <- A.ask
  result <- serviceCall(config).to[F]
} yield result