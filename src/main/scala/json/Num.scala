package com.github.aminmal
package json

import scala.util.Try

import error.ParseError

enum Num {
  case L(n: Long)
  case D(n: Double)
}

object Num {

  def fromString(s: String): Either[ParseError, Num] =
    if s.contains('.') then Try(s.mkString.toDouble).map(Num.D(_)).toEither.left.map(_ => ParseError.InvalidNumber("Double", s))
    else Try(s.toLong).map(Num.L(_)).toEither.left.map(_ => ParseError.InvalidNumber("Long", s))
}
