package core

import scala.annotation.tailrec
import scala.collection.mutable

import json.{IndexedJsValue, JsPointer}

object Indexer {

  private object NumChar {
    def unapply(c: Char): Boolean = c.isDigit
  }

  private object WhiteSpace {
    def unapply(c: Char): Boolean = c.isWhitespace
  }

  @tailrec private def parseStrTR(i: StrItr, s: StringBuilder = StringBuilder()): String =
    i.pop() match {
      case Some('"') | None => s.toString()
      case Some('\\')       =>
        i.pop().foreach(s.append)
        parseStrTR(i, s)
      case Some(other)      =>
        s.append(other)
        parseStrTR(i, s)
    }

  private def parseStr(i: StrItr): String =
    parseStrTR(i)

  @tailrec private def parseValue(i: StrItr): Either[ParseError, JsPointer] = {
    val head = i.peek()
    head match {
      case Some('t')                   =>
        if i.startsWith("true") then
          i.advance(4)
          Right(JsPointer.True)
        else
          Left(
            ParseError.UnexpectedToken(i.pos, List("true"))
          )
      case Some('f')                   =>
        if i.startsWith("false") then
          i.advance(5)
          Right(JsPointer.False)
        else
          Left(
            ParseError.UnexpectedToken(i.pos, List("false"))
          )
      case Some('n')                   =>
        if i.startsWith("null") then
          i.advance(4)
          Right(JsPointer.Null)
        else
          Left(
            ParseError.UnexpectedToken(i.pos, List("true"))
          )
      case Some(NumChar()) | Some('-') =>
        val startPos = i.pos
        i.advance()
        i.advanceWhile(c => NumChar.unapply(c) || c == '.')
        Right(JsPointer.N(startPos, i.pos))
      case Some('"')                   =>
        i.advance()
        Right(JsPointer.Str(parseStr(i)))
      case Some('{')                   => parseObj(i)
      case Some('[')                   => parseArr(i)
      case Some(WhiteSpace())          =>
        i.advance()
        parseValue(i)
      case c                           =>
        Left(ParseError.UnexpectedToken(i.pos, List("{", "[", "t", "f", "n", "-", "<0-9>", "\"")))
    }
  }

  private enum ObjectParseState {
    case ExpectingKey, ExpectingKeyOrEndOfObject, ExpectingCommaOrEndOfObject, ExpectingColon, ExpectingValue
  }

  private def parseObj(i: StrItr): Either[ParseError, JsPointer] = {
    val keyValues = mutable.Map.empty[String, JsPointer]
    i.advance() // pop {

    @tailrec def po(
      state: ObjectParseState = ObjectParseState.ExpectingKeyOrEndOfObject,
      latestKey: Option[String] = None
    ): Either[ParseError, JsPointer] =
      i.peek() match
        case None                    => Left(ParseError.EOF)
        case Some(' ' | '\t' | '\n') =>
          i.advance() // ignore whitespace
          po(
            state = state,
            latestKey = latestKey
          )
        case Some(next)              =>
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
                  Left(
                    ParseError.UnexpectedToken(
                      i.pos,
                      List("\"")
                    )
                  )

            case ObjectParseState.ExpectingKeyOrEndOfObject =>
              next match
                case '}' =>
                  i.advance()
                  Right(JsPointer.Obj(keyValues))
                case '"' =>
                  i.advance() // pop '"'
                  po(
                    state = ObjectParseState.ExpectingColon,
                    latestKey = Some(parseStr(i))
                  )
                case _   =>
                  Left(
                    ParseError.UnexpectedToken(
                      i.pos,
                      List("\"", "}")
                    )
                  )

            case ObjectParseState.ExpectingCommaOrEndOfObject =>
              next match
                case '}' =>
                  i.advance()
                  Right(JsPointer.Obj(keyValues))
                case ',' =>
                  i.advance()
                  po(
                    state = ObjectParseState.ExpectingKey,
                    latestKey = latestKey
                  )
                case _   =>
                  Left(
                    ParseError.UnexpectedToken(
                      i.pos,
                      List(",", "}")
                    )
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
                  Left(
                    ParseError.UnexpectedToken(
                      i.pos,
                      List(":")
                    )
                  )

            case ObjectParseState.ExpectingValue =>
              next match
                case _ =>
                  parseValue(i) match
                    case Left(err)    => Left(err)
                    case Right(value) =>
                      latestKey match
                        case Some(key) =>
                          keyValues.put(key, value)
                          po(
                            state = ObjectParseState.ExpectingCommaOrEndOfObject,
                            latestKey = None
                          )
                        case None      =>
                          Left(ParseError.InvalidJsonStructure)
          }

    po(ObjectParseState.ExpectingKeyOrEndOfObject, None)
  }

  private enum ArrParseState {
    case ExpectingValue, ExpectingValueOrEndOfArray, ExpectingCommaOrEndOfArray
  }

  private def parseArr(i: StrItr): Either[ParseError, JsPointer] = {
    val values = mutable.ListBuffer.empty[JsPointer]
    i.advance() // pop [

    @tailrec def pa(
      state: ArrParseState
    ): Either[ParseError, JsPointer] =
      i.peek() match {
        case None                    => Left(ParseError.EOF)
        case Some(' ' | '\t' | '\n') =>
          i.advance()
          pa(state = state)
        case Some(head)              =>
          state match {
            case ArrParseState.ExpectingValueOrEndOfArray =>
              head match
                case ']' =>
                  i.advance()
                  Right(JsPointer.Arr(values))
                case _   =>
                  parseValue(i) match
                    case Left(err)    => Left(err)
                    case Right(value) =>
                      values.append(value)
                      pa(state = ArrParseState.ExpectingCommaOrEndOfArray)

            case ArrParseState.ExpectingCommaOrEndOfArray =>
              head match
                case ']' =>
                  i.advance()
                  Right(JsPointer.Arr(values))
                case ',' =>
                  i.advance()
                  pa(state = ArrParseState.ExpectingValue)
                case _   =>
                  Left(
                    ParseError.UnexpectedToken(
                      i.pos,
                      List(",", "]")
                    )
                  )

            case ArrParseState.ExpectingValue =>
              head match
                case _ =>
                  parseValue(i) match
                    case Left(err)    => Left(err)
                    case Right(value) =>
                      values.append(value)
                      pa(state = ArrParseState.ExpectingCommaOrEndOfArray)
          }
      }

    pa(ArrParseState.ExpectingValueOrEndOfArray)
  }

  @tailrec private def isEffectivelyEmpty(i: StrItr): Boolean =
    i.peek() match {
      case None                      => true
      case Some(c) if c.isWhitespace =>
        i.advance()
        isEffectivelyEmpty(i)
      case Some(_)                   => false
    }

  inline def parse(raw: String): Either[ParseError, IndexedJsValue] = {
    val i = StrItr(raw, 0)
    parseValue(i) match {
      case Right(value) =>
        if isEffectivelyEmpty(i) then Right(IndexedJsValue(raw, value))
        else Left(ParseError.UnexpectedToken(i.pos, expected = List("EOF")))
      case Left(err)    => Left(err)
    }
  }
}
