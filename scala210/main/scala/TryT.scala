package scalaz.contrib
package std

import scala.util.control.NonFatal
import scala.util.{Try, Success, Failure}
import scalaz._
import utilTry._
import scalaz.syntax.foldable._

final case class TryT[F[+_], +A](run: F[Try[A]]){ self =>

  def isFailure(implicit F: Functor[F]): F[Boolean] = mapT(_.isFailure)

  def isSuccess(implicit F: Functor[F]): F[Boolean] = mapT(_.isSuccess)

  def getOrElse[AA >: A](default: => AA)(implicit F: Functor[F]): F[AA] = mapT(_.getOrElse(default))

  def orElse[AA >: A](a: => TryT[F, AA])(implicit F: Monad[F]): TryT[F, AA] = TryT(
    F.bind(run) {
      case Failure(_) => a.run
      case x @ Success(_) => F.point(x)
    }
  )

  def get(implicit F: Functor[F]): F[A] = mapT(_.get)

  def foreach[B](f: A => B)(implicit F: Each[F]): Unit = F.each(run)(_ foreach f)

  def flatMap[B](f: A => TryT[F, B])(implicit F: Monad[F]): TryT[F, B] = TryT[F, B](
    F.bind(self.run) {
      case e @ Failure(_) => F.point(e.asInstanceOf[Try[B]])
      case Success(z) => f(z).run
    }
  )

  def flatMapF[B](f: A => F[B])(implicit F: Monad[F]): TryT[F, B] = TryT[F, B](
    F.bind(self.run) {
      case e @ Failure(_) => F.point(e.asInstanceOf[Try[B]])
      case Success(z) => F.map(f(z))(Success(_))
    }
  )

  def map[B](f: A => B)(implicit F: Functor[F]): TryT[F, B] = TryT(mapT(_ map f))

  def filter(p: A => Boolean)(implicit F: Functor[F]): TryT[F, A] = TryT(mapT(_.filter(p)))

  def recoverWith[AA >: A](f: PartialFunction[Throwable, TryT[F, AA]])(implicit F: Monad[F]): TryT[F, AA] = TryT(
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

  def recover[AA >: A](f: PartialFunction[Throwable, AA])(implicit F: Functor[F]): TryT[F, AA] =
    TryT(mapT(_.recover(f)))

  def toOption(implicit F: Functor[F]): OptionT[F, A] =
    OptionT.optionT(F.map(run)(_.toOption))

  def failed(implicit F: Functor[F]): TryT[F, Throwable] = TryT(mapT(_.failed))

  def transform[B](s: A => TryT[F, B], f: Throwable => TryT[F, B])(implicit F: Monad[F]): TryT[F, B] = TryT[F, B](
    F.bind(self.run) {
      case Failure(e) => f(e).run
      case Success(z) => s(z).run
    }
  )

  def traverse[G[+_], B](f: A => G[B])(implicit F: Traverse[F], G: Applicative[G]): G[TryT[F, B]] =
    G.map(F.traverse(run)(o => Traverse[Try].traverse(o)(f)))(TryT(_))

  def foldRight[Z](z: => Z)(f: (A, => Z) => Z)(implicit F: Foldable[F]): Z =
    F.foldRight[Try[A], Z](run, z)((a, b) => a.foldRight(b)(f))

  private[this] def mapT[B](f: Try[A] => B)(implicit F: Functor[F]): F[B] = F.map(run)(f)
}

object TryT extends TryTInstances

trait TryTInstances{

}

private[scalaz] trait TryTFunctor[F[+_]] extends Functor[({type λ[α] = TryT[F, α]})#λ] {
  implicit def F: Functor[F]

  override def map[A, B](fa: TryT[F, A])(f: A => B): TryT[F, B] = fa map f
}

private[scalaz] trait TryTMonad[F[+_]] extends Monad[({type λ[α] = TryT[F, α]})#λ] with TryTFunctor[F]{
  implicit def F: Monad[F]

  override def point[A](a: => A): TryT[F, A] = TryT(F.point(Success(a)))

  override def bind[A, B](fa: TryT[F, A])(f: A => TryT[F, B]): TryT[F, B] = fa flatMap f
}

private[scalaz] trait TryTTraverse[F[+_]] extends Traverse[({type λ[α] = TryT[F, α]})#λ] with TryTFunctor[F]{
  implicit def F: Traverse[F]

  override def foldRight[A, B](fa: TryT[F, A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z)(f)

  override def traverseImpl[G[+_]: Applicative, A, B](fa: TryT[F, A])(f: A => G[B]): G[TryT[F, B]] = fa traverse f
}

