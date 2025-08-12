package com.github.aminmal
package error

enum ParseError {
  case EOF
  case UnexpectedToken(pos: Int, expected: List[String])
  case InvalidNumber(tpe: String, value: String)
  case InvalidJsonStructure
}
