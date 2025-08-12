package com.github.aminmal
package fp.syntax

import fp.Monad

trait MonadSyntax {

  extension [F[_]: Monad, A](value: F[A]) {
    inline def flatMap[B](f: A => F[B]): F[B] = summon[Monad[F]].flatMap(value)(f)
  }
}
