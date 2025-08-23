package core

import scala.annotation.tailrec

class StrItr(val raw: String, var pos: Int, var cp: Int = -1) {
  private final val len           = raw.length
  inline def peek(): Option[Char] = if hasNext then Some(raw(pos)) else None

  inline def take(n: Int): String = raw.slice(pos, pos + n)

  inline def hasNext: Boolean = pos < len

  inline def checkpoint(): Unit =
    if cp != -1 then throw new RuntimeException("Cannot set more than one checkpoint at a time")
    else cp = pos

  inline def commitCheckpoint(): Unit =
    cp = -1

  inline def resetCheckpoint(): Unit = {
    pos = cp
    cp = -1
  }

  def takeWhile(p: Char => Boolean): String = {
    @tailrec def matchCounterTR(matchCount: Int = 0): Int =
      peek() match {
        case Some(c) if p(c) =>
          advance()
          matchCounterTR(matchCount + 1)
        case _               =>
          matchCount
      }
    raw.slice(pos, pos + matchCounterTR())
  }

  def advanceWhile(p: Char => Boolean): Unit = {
    @tailrec def matchCounterTR(): Unit =
      peek() match {
        case Some(c) if p(c) =>
          advance()
          matchCounterTR()
        case _               =>
          ()
      }

    matchCounterTR()
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
