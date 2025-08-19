package core

enum ParseError {
  case EOF
  case UnexpectedToken(pos: Int, expected: List[String])
  case InvalidNumber(tpe: String, value: String)
  case InvalidJsonStructure

  override def toString: String = this match
    case ParseError.EOF                            => "Unexpected EOF"
    case ParseError.UnexpectedToken(pos, expected) => s"Unexpected token at pos: $pos, expected: ${expected.map(s => s"'$s'")}"
    case ParseError.InvalidNumber(tpe, value)      => s"Invalid number $value for type $tpe"
    case ParseError.InvalidJsonStructure           => "Invalid Json structure"
}
