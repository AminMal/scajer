package com.github.aminmal
package fp

trait Monad[F[_]] {
  def pure[T](t: T): F[T]
  def flatMap[A, B](value: F[A])(f: A => F[B]): F[B]
}
