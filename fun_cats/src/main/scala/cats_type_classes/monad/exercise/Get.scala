package cats_type_classes.monad.exercise

import cats._
import cats.implicits._
import org.scalacheck.Gen

import java.nio.{ByteBuffer, ByteOrder}

case class Get[A] (run: List[Byte] => Either[String, (List[Byte], A)])

object Get {
  /**
   * TODO 1
   * Consumes n bytes of input parsing no value.
   */
  def skip(n: Int): Get[Unit] = Get { bytes =>
    if (bytes.length < n) Left("Insufficient input")
    else Right(bytes.drop(n), ())
  }

  /**
   * TODO 2
   * True if the input is fully consumed
   */
  def isEmpty: Get[Boolean] = Get { bytes =>
    Right(bytes, bytes.isEmpty)
  }

  /**
   * TODO 3
   * Reads one byte from input
   */
  def getByte: Get[Byte] = Get {
    case h :: t => Right(t, h)
    case Nil => Left("Insufficient input")
  }

  /**
   * TODO 4
   * Reads an Int from input using Big Endian order.
   */
  def getIntBE: Get[Int] = getByte.replicateA(4).map(bytes =>
    bytesToIntUnsafe(bytes.toArray, ByteOrder.BIG_ENDIAN)
  )

  /**
   * TODO 5
   * Reads an Int from input using Little Endian order.
   */
  def getIntLE: Get[Int] = getByte.replicateA(4).map(bytes =>
    bytesToIntUnsafe(bytes.toArray, ByteOrder.LITTLE_ENDIAN)
  )

  /**
   * TODO 6
   * Reads a String of n characters from input.
   */
  def getString(n: Int): Get[String] = getByte.replicateA(n).map(bytes =>
    new String(bytes.toArray)
  )

  /**
   * Helper function that turns four bytes into an Int. It doesn't check the
   * length of the array, so please make sure to provide 4 bytes.
   */
  private def bytesToIntUnsafe(fourBytes: Array[Byte], order: ByteOrder): Int = {
    val bb = ByteBuffer.allocate(4).order(order)
    bb.put(fourBytes)
    bb.flip()
    bb.getInt()
  }

  /**
   * TODO 7
   * Instance of monad error for Get. A rather difficult question
   */
  implicit val monadGet: MonadError[Get, String] = new MonadError[Get, String] {
    override def flatMap[A, B](fa: Get[A])(f: A => Get[B]): Get[B] = Get {
      bytes =>
        fa.run(bytes) match {
          case Left(value) => Left(value)
          case Right((remainingBytes, a)) => f(a).run(remainingBytes)
        }
    }

    override def pure[A](a: A): Get[A] =
      Get {
        bytes => Right((bytes, a))
      }

    override def tailRecM[A, B](a: A)(f: A => Get[Either[A, B]]): Get[B] = {
      Get { bytes =>
        Monad[Either[String, *]].tailRecM((bytes, a)) { case (bytes, a) =>
          f(a).run(bytes).map { case (bytes, eab) =>
            eab match {
              case Right(b) => Right((bytes, b))
              case Left(a) => Left((bytes, a))
            }
          }
        }
      }
    }

    override def raiseError[A](e: String): Get[A] =
      Get {
        _ => Left(e)
      }

    override def handleErrorWith[A](fa: Get[A])(f: String => Get[A]): Get[A] =
      Get {
        bytes =>
          fa.run(bytes) match {
            case Left(value) => f(value).run(bytes)
            case Right(value) => Right(value)
          }
      }
  }

  /**
   * TODO 8
   * Instance of Eq for Get. A full comparison is impossible, so we just
   * compare on a given number of List[Byte] samples and assume that
   * if both Get compute the same result, they are equal.
   *
   * Hint: One possible way of doing this is to use scalacheck to build
   * a generator of List[Byte], then sample it several times (e.g. 32)
   * and check that running both Gets yields the same result every time.
   */
  implicit def eqGet[A: Eq]: Eq[Get[A]] = new Eq[Get[A]] {
    override def eqv(x: Get[A], y: Get[A]): Boolean = {
      val genBytes = Gen.numStr.map(str => str.getBytes.toList)
      Iterator
        .continually(genBytes.sample)
        .take(32)
        .flatten
        .forall(bytes => x.run(bytes) == y.run(bytes))
    }

  }

  /**
   * TODO 9
   * Monoid instance for Get.
   */
  implicit def monoid[A: Monoid]: Monoid[Get[A]] = new Monoid[Get[A]] {
    override def empty: Get[A] = /*Get {
      Right(_, implicitly[Monoid[A]].empty)
    }*/
      Monad[Get].pure(Monoid[A].empty)

    override def combine(x: Get[A], y: Get[A]): Get[A] = { /*Get { bytes =>
      (x.run(bytes), y.run(bytes)) match {
        case (Left(x), Right(_)) => Left(x)
        case (Left(x), Left(y)) => Left(x + y)
        case (Right(_), Left(y)) => Left(y)
        case (Right((xs, xa)), Right((ys, ya))) => Right(xs ++ ys, xa |+| ya)
      }
    }*/
      (x, y).mapN((a1, a2) => a1 |+| a2)
    }
  }
}