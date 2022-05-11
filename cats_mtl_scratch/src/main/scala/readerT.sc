import cats._
import cats.data._
import cats.implicits._
import cats.effect._

// These are just String for simplicity
type Config = String
type Result = String

def getConfig: IO[Config] = ???
// getConfig: cats.effect.IO[Config]

def serviceCall(c: Config): IO[Result] = ???
// serviceCall: (c: Config)cats.effect.IO[Result]

def readerProgram: ReaderT[IO, Config, Result] = for {
  config <- ReaderT.ask[IO, Config]
  result <- ReaderT.liftF(serviceCall(config))
} yield result
// readerProgram: cats.data.ReaderT[cats.effect.IO,Config,Result]

def main: IO[Result] = getConfig.flatMap(readerProgram.run)