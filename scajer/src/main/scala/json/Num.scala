package scajer.json

import scajer.core.ParseError

enum Num {
  case L(n: Long)
  case D(n: Double)
}

object Num {

  def fromString(s: String): Num =
    if s.contains('.') then
      try Num.D(s.toDouble)
      catch {
        case _: NumberFormatException => throw ParseError.InvalidNumber("Double", s)
      }
    else {
      try Num.L(s.toLong)
      catch {
        case _: NumberFormatException => throw ParseError.InvalidNumber("Long", s)
      }
    }
}
