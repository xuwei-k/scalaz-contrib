package scalaz.contrib
package std

import scala.util.control.NonFatal
import scala.util.{Try, Success, Failure}
import scalaz._

final case class TryT[F[+_], +T](run: F[Try[T]]){ self =>

  def isFailure(implicit F: Functor[F]): F[Boolean] = mapT(_.isFailure)

  def isSuccess(implicit F: Functor[F]): F[Boolean] = mapT(_.isSuccess)

  def getOrElse[U >: T](default: => U)(implicit F: Functor[F]): F[U] = mapT(_.getOrElse(default))

  def orElse[U >: T](a: => TryT[F, U])(implicit F: Monad[F]): TryT[F, U] = TryT(
    F.bind(run) {
      case Failure(_) => a.run
      case x @ Success(_) => F.point(x)
    }
  )

  def get(implicit F: Functor[F]): F[T] = mapT(_.get)

  def foreach[U](f: T => U)(implicit F: Each[F]): Unit = F.each(run)(_ foreach f)

  def flatMap[U](f: T => TryT[F, U])(implicit F: Monad[F]): TryT[F, U] = TryT[F, U](
    F.bind(self.run) {
      case e @ Failure(_) => F.point(e.asInstanceOf[Try[U]])
      case Success(z) => f(z).run
    }
  )

  def flatMapF[U](f: T => F[U])(implicit F: Monad[F]): TryT[F, U] = TryT[F, U](
    F.bind(self.run) {
      case e @ Failure(_) => F.point(e.asInstanceOf[Try[U]])
      case Success(z) => F.map(f(z))(Success(_))
    }
  )

  def map[U](f: T => U)(implicit F: Functor[F]): TryT[F, U] = TryT(mapT(_ map f))

  def filter(p: T => Boolean)(implicit F: Functor[F]): TryT[F, T] = TryT(mapT(_.filter(p)))

  def recoverWith[U >: T](f: PartialFunction[Throwable, TryT[F, U]])(implicit F: Monad[F]): TryT[F, U] = TryT(
    F.bind(self.run) {
      case x @ Failure(e) =>
        try {
          if (f isDefinedAt e) f(e).run else F.point(x)
        } catch {
          case NonFatal(e) => F.point(Failure(e))
        }
      case z @ Success(_) => F.point(z)
    }
  )

  def recover[U >: T](f: PartialFunction[Throwable, U])(implicit F: Functor[F]): TryT[F, U] =
    TryT(mapT(_.recover(f)))

  def toOption(implicit F: Functor[F]): OptionT[F, T] =
    OptionT.optionT(F.map(run)(_.toOption))

  def failed(implicit F: Functor[F]): TryT[F, Throwable] = TryT(mapT(_.failed))

  def transform[U](s: T => TryT[F, U], f: Throwable => TryT[F, U])(implicit F: Monad[F]): TryT[F, U] = TryT[F, U](
    F.bind(self.run) {
      case Failure(e) => f(e).run
      case Success(z) => s(z).run
    }
  )

  private[this] def mapT[B](f: Try[T] => B)(implicit F: Functor[F]): F[B] = F.map(run)(f)
}

