import cats._
import cats.implicits._

import java.io.IOException
import scala.util._

trait HttpMethod
case object GET extends HttpMethod
case class HttpRequest(method: HttpMethod, url: String)
case class HttpResponse(status: Int)

implicit val optionMonadError: MonadError[Option, Unit] = new MonadError[Option, Unit] {
  override def pure[A](a: A): Option[A] = Some(a)

  override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = ???

  override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]) = ???

  override def raiseError[A](e: Unit): Option[A] = None

  override def handleErrorWith[A](fa: Option[A])(f: Unit => Option[A]): Option[A] =
    fa.orElse(f())
}

def doRequest(req: HttpRequest): HttpResponse = {
  if (math.random() < 0.5) throw new IOException("Boom")
  else HttpResponse(200)
}

def executeRequestME[F[_]](req: HttpRequest)(implicit me: MonadError[F, Throwable]): F[HttpResponse] =
//def executeRequestME[F[_]: MonadError[F, Throwable]](req: HttpRequest): F[HttpResponse] =
  try {
//    implicitly[MonadError[F, Throwable]].pure(doRequest(req))
    me.pure(doRequest(req))
//    doRequest(req).pure[F]
  } catch {
    case e: Exception => me.raiseError(e)
  }

executeRequestME[Try](HttpRequest(GET, "www.example.com"))

type ErrorOr[A] = Either[Throwable, A]
executeRequestME[ErrorOr](HttpRequest(GET, "www.example.com"))

def executeRequestME[F[_]: MonadThrow](req: HttpRequest)/*(implicit ME: MonadError[F, E])*/: F[HttpResponse] =
  try {
    implicitly[MonadThrow[F]].pure(doRequest(req))
  } catch {
    case e: Exception => implicitly[MonadThrow[F]].raiseError(e)/*.raiseError(f(e))*/
  }

executeRequestME[ErrorOr](HttpRequest(GET, "www.eexample.com"))

MonadError[ErrorOr, Throwable].ensure(Right(6))(throw new Exception("Oh noes"))(_ % 2 == 0)

def executeRequestME[F[_], E](req: HttpRequest)(f: Exception => E)(implicit ME: MonadError[F, E]): F[HttpResponse] =
  try {
    ME.pure(doRequest(req))
  } catch {
    case e: Exception => ME.raiseError(f(e))
  }

executeRequestME[Option, Unit](HttpRequest(GET, "www.eexample.com"))((_: Exception) => ())
executeRequestME[ErrorOr, Throwable](HttpRequest(GET, "www.eexample.com"))(_)
