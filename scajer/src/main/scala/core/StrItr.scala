package core

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

  inline def advance(n: Int = 1): Unit = pos += n

  def pop(): Option[Char] = {
    val value = peek()
    advance()
    value
  }

  def startsWith(other: String): Boolean = {
    var i   = 0
    val len = other.length
    while i < len do {
      if other(i) != raw(pos + i) then return false
      i += 1
    }
    true
  }
}
