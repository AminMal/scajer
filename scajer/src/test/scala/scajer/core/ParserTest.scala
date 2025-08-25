package scajer
package core

import munit.FunSuite
import scajer.json.{JsValue, Num}

class ParserTest extends FunSuite {

  test("NumChar.unapply should return true for digit chars, and false for non-digits") {
    val digits    = List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
    assert(digits.forall(Parser.NumChar.unapply))
    val nonDigits = ('a' to 'z').toList ::: ('A' to 'Z').toList ::: ((0: Char) to ' ').toList
    assert(nonDigits.forall(c => !Parser.NumChar.unapply(c)))
  }

  test("WhiteSpace.unapply should return true for white space chars") {
    val spaces = List(' ', '\n', '\t', '\r')
    assert(spaces.forall(Parser.WhiteSpace.unapply))
  }

  test("Parser.parseSimpleStr should parse non-escaped string literals, returning the index where they end") {
    val i = StrItr.init(":\"literal string between dquotes\",...")
    i.advance(2) // : and starting "
    val endPos         = Parser.parseSimpleStr(i)
    val expectedEndPos = i.raw.lastIndexOf('\"') + 1
    assertEquals(endPos, expectedEndPos)
  }

  test("Parser.parseSimpleStr should return -1 if the string is not simple") {
    val i = StrItr.init(":\"This one has \\n escaped chars\",...")
    i.advance(2) // : and starting "
    assertEquals(Parser.parseSimpleStr(i), -1)
  }

  test("Parser.parseSimpleStr should throw exception when reaching eof") {
    val i = StrItr.init(":\"This one has no escaped chars and doesn't close")
    i.advance(2) // : and starting "
    interceptMessage[ParseError]("Unexpected EOF")(Parser.parseSimpleStr(i))
  }

  test("Parser.parseSimpleStr should throw exception when invalid character is found") {
    val c: Char = 10
    val i       = StrItr.init(s":\"literal string between dquotes with strange symbol: $c\",...")
    i.advance(2) // : and starting "
    val expectedMessage = s"Invalid character $c at position 55"
    interceptMessage[ParseError](expectedMessage)(Parser.parseSimpleStr(i))
  }

  test("Parser.parseStrTR should parse next string literal") {
    val i = StrItr.init(":\"This one has \\n escaped chars \\t\\r\",...")
    i.advance(2) // : and starting "
    val s = Parser.parseStrTR(i)
    assertEquals(s, "This one has \n escaped chars \t\r")
  }

  test("Parser.parseStrTR should throw exception when reaching eof") {
    val i = StrItr.init(":\"This one has \\n escaped chars \\t\\r,...")
    i.advance(2) // : and starting "
    interceptMessage[ParseError]("Unexpected EOF")(Parser.parseStrTR(i))
  }

  test("Parser.parseStrTR should throw exception when invalid character is found") {
    val c: Char = 10
    val i       = StrItr.init(s":\"This one has \\n escaped chars \\t\\r with this strange symbol: $c \",...")
    i.advance(2) // : and starting "
    val expectedMessage = s"Invalid character $c at position 64"
    interceptMessage[ParseError](expectedMessage)(Parser.parseStrTR(i))
  }

  test("Parser.parseStrTR should throw exception when invalid escape character is passed") {
    val i = StrItr.init(""":"This one has invalid esac\pe char",...""")
    i.advance(2) // : and starting "
    val expectedMessage = "Invalid escape character p at position 29"
    interceptMessage[ParseError](expectedMessage)(Parser.parseStrTR(i))
  }

  test("Parser.parseNumber should parse valid integer number") {
    val i        = StrItr.init("29,...")
    val got      = Parser.parseNumber(i)
    val expected = JsValue.JsNumber(Num.L(29))
    assertEquals(expected, got)
  }

  test("Parser.parseNumber should parse valid negative integer number") {
    val i        = StrItr.init("-29,...")
    val got      = Parser.parseNumber(i)
    val expected = JsValue.JsNumber(Num.L(-29))
    assertEquals(expected, got)
  }

  test("Parser.parseNumber should parse valid double number") {
    val i        = StrItr.init("29.876,...")
    val got      = Parser.parseNumber(i)
    val expected = JsValue.JsNumber(Num.D(29.876))
    assertEquals(expected, got)
  }

  test("Parser.parseNumber should parse valid negative double number") {
    val i        = StrItr.init("-29.876,...")
    val got      = Parser.parseNumber(i)
    val expected = JsValue.JsNumber(Num.D(-29.876))
    assertEquals(expected, got)
  }

  test("Parser.parseNumber should not parse invalid double number") {
    val i = StrItr.init("29.876.124,...")
    interceptMessage[ParseError]("Invalid number 29.876.124 for type Double")(Parser.parseNumber(i))

    val i2 = StrItr.init(".876.124,...")
    interceptMessage[ParseError]("Invalid number .876.124 for type Double")(Parser.parseNumber(i2))
  }

  test("Parser.parseValue should parse valid boolean") {
    val t = StrItr.init("true,...")
    val f = StrItr.init("false,...")

    assertEquals(Parser.parseValue(t), JsValue.JsBool(true))
    assertEquals(Parser.parseValue(f), JsValue.JsBool(false))
  }

  test("Parser.parseValue should throw exception when expecting certain boolean characters but got other chars") {
    val t = StrItr.init("trie,...")
    val f = StrItr.init("falze,...")

    interceptMessage[ParseError]("Unexpected token at pos: 0, expected: List('true')")(Parser.parseValue(t))
    interceptMessage[ParseError]("Unexpected token at pos: 0, expected: List('false')")(Parser.parseValue(f))
  }

  test("Parser.parseValue should parse null") {
    val n = StrItr.init("null,...")
    assertEquals(Parser.parseValue(n), JsValue.JsNull)
  }

  test("Parser.parseValue should throw exception when expecting certain boolean characters but got other chars") {
    val n = StrItr.init("nufl,...")
    interceptMessage[ParseError]("Unexpected token at pos: 0, expected: List('null')")(Parser.parseValue(n))
  }

  test("Parser.parseValue should parse numbers") {
    val i1     = StrItr.init("32,...")
    val value1 = Parser.parseValue(i1)

    val i2     = StrItr.init("-32.3,...")
    val value2 = Parser.parseValue(i2)

    assertEquals(value1, JsValue.JsNumber(Num.L(32)))
    assertEquals(value2, JsValue.JsNumber(Num.D(-32.3)))
  }

  test("Parser.parseValue should parse string") {
    val i     = StrItr.init("\"This is a value\",...")
    val value = Parser.parseValue(i)

    assertEquals(value, JsValue.JsString("This is a value"))
  }

  test("Parser.parseValue should escape whitespaces") {
    val i     = StrItr.init("\n\t    \"This is a value\",...")
    val value = Parser.parseValue(i)

    assertEquals(value, JsValue.JsString("This is a value"))
  }

  test("Parser.parseObj should parse valid object") {
    val raw = """{"name": "John",    "age": 25}"""
    val i   = StrItr.init(raw)

    val JsValue.JsObject(inner) = Parser.parseObj(i): @unchecked
    assertEquals(inner, scala.collection.mutable.HashMap("name" -> JsValue.JsString("John"), "age" -> JsValue.JsNumber(Num.L(25))))
  }

  test("Parser.parseObj should throw an exception when invalid JSON object is passed #1") {
    val raw = """{"name": "John", 25}"""
    val i   = StrItr.init(raw)

    interceptMessage[ParseError]("Unexpected token at pos: 17, expected: List('<key>')")(Parser.parseObj(i))
  }

  test("Parser.parseObj should throw an exception when invalid JSON object is passed #2") {
    val raw = """{ 25"""
    val i   = StrItr.init(raw)

    interceptMessage[ParseError]("Unexpected token at pos: 2, expected: List('<key>', '}')")(Parser.parseObj(i))
  }

  test("Parser.parseObj should throw an exception when invalid JSON object is passed #3") {
    val raw = """{"name": "John" 25}"""
    val i   = StrItr.init(raw)

    interceptMessage[ParseError]("Unexpected token at pos: 16, expected: List(',', '}')")(Parser.parseObj(i))
  }

  test("Parser.parseObj should throw an exception when invalid JSON object is passed #4") {
    val raw = """{"name" "John"}"""
    val i   = StrItr.init(raw)

    interceptMessage[ParseError]("Unexpected token at pos: 8, expected: List(':')")(Parser.parseObj(i))
  }

  test("Parser.parseArr should parse valid JSON array") {
    val raw = """[null, false, true, 2, 1.2, "name", {"name":"John"}, []]"""
    val i   = StrItr.init(raw)

    val value = Parser.parseArr(i)
    assertEquals(
      value,
      JsValue.JsArray(
        scala.collection.mutable.ListBuffer(
          JsValue.JsNull,
          JsValue.JsBool(false),
          JsValue.JsBool(true),
          JsValue.JsNumber(Num.L(2)),
          JsValue.JsNumber(Num.D(1.2)),
          JsValue.JsString("name"),
          JsValue.JsObject(
            scala.collection.mutable.HashMap("name" -> JsValue.JsString("John"))
          ),
          JsValue.JsArray(scala.collection.mutable.ListBuffer())
        )
      )
    )
  }

  test("Parser.parseArr should throw proper exception when parsing invalid JSON array #1") {
    val raw = """[null, ]"""
    val i   = StrItr.init(raw)

    interceptMessage[ParseError]("Unexpected token at pos: 7, expected: List('{', '[', 'true', 'false', 'null', '-', '<0-9>', '\"')")(
      Parser.parseArr(i)
    )
  }

  test("Parser.parseArr should throw proper exception when parsing invalid JSON array #2") {
    val raw = """[null "string"]"""
    val i   = StrItr.init(raw)

    interceptMessage[ParseError]("Unexpected token at pos: 6, expected: List(',', ']')")(Parser.parseArr(i))
  }

}
