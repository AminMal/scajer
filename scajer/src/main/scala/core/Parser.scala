package core

import scala.annotation.{switch, tailrec}
import scala.collection.mutable

import json.{JsValue, Num}

object Parser {

  private object NumChar {
    def unapply(c: Char): Boolean = c.isDigit
  }

  private object WhiteSpace {
    def unapply(c: Char): Boolean = c.isWhitespace
  }

  @tailrec private def parseSimpleStr(i: StrItr): Int =
    (i.pop(): @switch) match {
      case Some(c) if c < ' ' => throw ParseError.InvalidCharacter(i.pos, c)
      case Some('\\')         => -1
      case Some('"')          => i.pos
      case Some(_)            => parseSimpleStr(i)
      case None               => throw ParseError.EOF
    }

  private def smartParseStr(i: StrItr): String = {
    val start = i.pos
    i.checkpoint()
    (parseSimpleStr(i): @switch) match {
      case -1     =>
        i.resetCheckpoint()
        parseStrTR(i)
      case endPos =>
        i.commitCheckpoint()
        i.raw.substring(start, endPos - 1)
    }
  }

  @tailrec private def parseStrTR(i: StrItr, s: StringBuilder = StringBuilder()): String =
    (i.pop(): @switch) match {
      case None        => throw ParseError.EOF
      case Some('"')   => s.toString()
      case Some('\\')  =>
        i.pop() match {
          case Some('b')   => s.append('\b')
          case Some('f')   => s.append('\f')
          case Some('n')   => s.append('\n')
          case Some('r')   => s.append('\r')
          case Some('t')   => s.append('\t')
          case Some('"')   => s.append('"')
          case Some('/')   => s.append('/')
          case Some('\\')  => s.append('\\')
          case Some('u')   => throw new RuntimeException("Unicode not implemented yet")
          case Some(other) => throw ParseError.InvalidEscapeCharacter(i.pos, other)
          case None        => throw ParseError.EOF
        }
        parseStrTR(i, s)
      case Some(other) =>
        s.append(other)
        parseStrTR(i, s)
    }

  private inline def parseStr(i: StrItr): String =
    smartParseStr(i)

  private def parseNumber(i: StrItr): JsValue = {
    val startPos = i.pos

    @tailrec def parseNumberTr: JsValue =
      (i.peek(): @switch) match {
        case Some('.') | Some(NumChar()) =>
          i.advance()
          parseNumberTr
        case _                           =>
          JsValue.JsNumber(Num.fromString(i.raw.substring(startPos, i.pos)))
      }

    i.peek() match {
      case Some('-') =>
        i.advance()
        parseNumberTr
      case Some(_)   =>
        parseNumberTr
      case None      => throw ParseError.EOF
    }
  }

  @tailrec private def parseValue(i: StrItr): JsValue =
    (i.peek(): @switch) match {
      case Some('t')                   =>
        if i.startsWith("true") then
          i.advance(4)
          JsValue.JsBool(true)
        else throw ParseError.UnexpectedToken(i.pos, List("true"))
      case Some('f')                   =>
        if i.startsWith("false") then
          i.advance(5)
          JsValue.JsBool(false)
        else throw ParseError.UnexpectedToken(i.pos, List("false"))
      case Some('n')                   =>
        if i.startsWith("null") then
          i.advance(4)
          JsValue.JsBool(true)
        else throw ParseError.UnexpectedToken(i.pos, List("true"))
      case Some(NumChar()) | Some('-') => parseNumber(i)
      case Some('"')                   =>
        i.advance()
        JsValue.JsString(parseStr(i))
      case Some('{')                   => parseObj(i)
      case Some('[')                   => parseArr(i)
      case Some(WhiteSpace())          =>
        i.advance()
        parseValue(i)
      case c                           =>
        throw ParseError.UnexpectedToken(i.pos, List("{", "[", "t", "f", "n", "-", "<0-9>", "\""))
    }

  private enum ObjectParseState {
    case ExpectingKey, ExpectingKeyOrEndOfObject, ExpectingCommaOrEndOfObject, ExpectingColon, ExpectingValue
  }

  private def parseObj(i: StrItr): JsValue = {
    val keyValues = mutable.HashMap.empty[String, JsValue]
    i.advance() // pop {

    @tailrec def po(
      state: ObjectParseState = ObjectParseState.ExpectingKeyOrEndOfObject,
      latestKey: Option[String] = None
    ): JsValue =
      i.peek() match
        case None               => throw ParseError.EOF
        case Some(WhiteSpace()) =>
          i.advance() // ignore whitespace
          po(
            state = state,
            latestKey = latestKey
          )
        case Some(next)         =>
          state match {
            case ObjectParseState.ExpectingKey =>
              next match
                case '"' =>
                  i.advance() // pop '"'
                  po(
                    state = ObjectParseState.ExpectingColon,
                    latestKey = Some(parseStr(i))
                  )
                case _   =>
                  throw ParseError.UnexpectedToken(
                    i.pos,
                    List("\"")
                  )

            case ObjectParseState.ExpectingKeyOrEndOfObject =>
              next match
                case '}' =>
                  i.advance()
                  JsValue.JsObject(keyValues)
                case '"' =>
                  i.advance() // pop '"'
                  po(
                    state = ObjectParseState.ExpectingColon,
                    latestKey = Some(parseStr(i))
                  )
                case _   =>
                  throw ParseError.UnexpectedToken(
                    i.pos,
                    List("\"", "}")
                  )

            case ObjectParseState.ExpectingCommaOrEndOfObject =>
              next match
                case '}' =>
                  i.advance()
                  JsValue.JsObject(keyValues)
                case ',' =>
                  i.advance()
                  po(
                    state = ObjectParseState.ExpectingKey,
                    latestKey = latestKey
                  )
                case _   =>
                  throw ParseError.UnexpectedToken(
                    i.pos,
                    List(",", "}")
                  )

            case ObjectParseState.ExpectingColon =>
              next match
                case ':' =>
                  i.advance()
                  po(
                    state = ObjectParseState.ExpectingValue,
                    latestKey = latestKey
                  )
                case _   =>
                  throw ParseError.UnexpectedToken(
                    i.pos,
                    List(":")
                  )

            case ObjectParseState.ExpectingValue =>
              next match
                case _ =>
                  val value = parseValue(i)
                  latestKey match {
                    case Some(key) =>
                      keyValues.put(key, value)
                      po(
                        state = ObjectParseState.ExpectingCommaOrEndOfObject,
                        latestKey = None
                      )
                    case None      => throw ParseError.InvalidJsonStructure
                  }
          }

    po(ObjectParseState.ExpectingKeyOrEndOfObject, None)
  }

  private enum ArrParseState {
    case ExpectingValue, ExpectingValueOrEndOfArray, ExpectingCommaOrEndOfArray
  }

  private def parseArr(i: StrItr): JsValue = {
    val values = mutable.ListBuffer.empty[JsValue]
    i.advance() // pop [

    @tailrec def pa(
      state: ArrParseState
    ): JsValue =
      i.peek() match {
        case None               => throw ParseError.EOF
        case Some(WhiteSpace()) =>
          i.advance()
          pa(state = state)
        case Some(head)         =>
          state match {
            case ArrParseState.ExpectingValueOrEndOfArray =>
              head match
                case ']' =>
                  i.advance()
                  JsValue.JsArray(values)
                case _   =>
                  val value = parseValue(i)
                  values.append(value)
                  pa(state = ArrParseState.ExpectingCommaOrEndOfArray)

            case ArrParseState.ExpectingCommaOrEndOfArray =>
              head match
                case ']' =>
                  i.advance()
                  JsValue.JsArray(values)
                case ',' =>
                  i.advance()
                  pa(state = ArrParseState.ExpectingValue)
                case _   =>
                  throw ParseError.UnexpectedToken(
                    i.pos,
                    List(",", "]")
                  )

            case ArrParseState.ExpectingValue =>
              head match
                case _ =>
                  val value = parseValue(i)
                  values.append(value)
                  pa(state = ArrParseState.ExpectingCommaOrEndOfArray)
          }
      }

    pa(ArrParseState.ExpectingValueOrEndOfArray)
  }

  @tailrec private def isEffectivelyEmpty(i: StrItr): Boolean =
    (i.peek(): @switch) match {
      case None                      => true
      case Some(c) if c.isWhitespace =>
        i.advance()
        isEffectivelyEmpty(i)
      case Some(_)                   => false
    }

  def parse(raw: String): Either[ParseError, JsValue] = {
    val i = StrItr(raw, 0)
    try {
      val value = parseValue(i)
      if isEffectivelyEmpty(i) then Right(value)
      else Left(ParseError.UnexpectedToken(i.pos, expected = List("EOF")))
    } catch {
      case err: ParseError => Left(err)
    }
  }
}
