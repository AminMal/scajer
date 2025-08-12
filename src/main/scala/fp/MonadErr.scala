package com.github.aminmal
package fp

trait MonadErr[F[_], E] extends Monad[F] {
  def raiseError[A](e: E): F[A]
  def recoverWith[A](value: F[A])(f: PartialFunction[E, F[A]]): F[A]

  def recover[A](value: F[A])(f: PartialFunction[E, A]): F[A] =
    recoverWith(value) { case e =>
      pure(f(e))
    }
}
