package com.github.aminmal
package core

import scala.annotation.tailrec

class StrItr(val raw: String, var pos: Int) {
  private final val len           = raw.length
  inline def peek(): Option[Char] = if hasNext() then Some(raw(pos)) else None

  inline def take(n: Int): String = raw.slice(pos, pos + n)

  inline def hasNext(): Boolean = pos < len

  def takeWhile(p: Char => Boolean): String = {
    @tailrec def matchCountrTR(matchCount: Int = 0): Int =
      peek() match {
        case Some(c) if p(c) =>
          advance()
          matchCountrTR(matchCount + 1)
        case _               =>
          matchCount
      }
    raw.slice(pos, pos + matchCountrTR())
  }

  inline def advance(n: Int = 1): Unit = pos += n

  def pop(): Option[Char] = {
    val value = peek()
    advance()
    value
  }

  def startsWith(other: String): Boolean = raw.drop(pos).startsWith(other)

  def getStringFromSlice(start: Int, end: Int): String = String.valueOf(raw.slice(start, end))
}
