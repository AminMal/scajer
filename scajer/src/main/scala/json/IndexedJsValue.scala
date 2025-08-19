package json

import core.ParseError

case class IndexedJsValue(original: String, ptr: JsPointer) {

  private def toJsValueImpl(p: JsPointer = ptr): ParseError | JsValue = p match
    case JsPointer.True           => JsValue.JsBool(true)
    case JsPointer.False          => JsValue.JsBool(false)
    case JsPointer.Null           => JsValue.JsNull
    case JsPointer.Str(s)         => JsValue.JsString(s)
    case JsPointer.N(start, end)  =>
      Num.fromString(original.slice(start, end)) match {
        case Left(pe) => pe
        case Right(n) => JsValue.JsNumber(n)
      }
    case JsPointer.Obj(keyValues) =>
      val inner = keyValues
        .map { (key, valuePtr) =>
          key -> toJsValueImpl(valuePtr)
        }
        .collect { case (k, v: JsValue) =>
          k -> v
        }
      JsValue.JsObject(inner)
    case JsPointer.Arr(values)    =>
      val inner = values
        .map(valuePtr => toJsValueImpl(valuePtr))
        .collect { case v: JsValue =>
          v
        }
      JsValue.JsArray(inner)

  def toJsValue: ParseError | JsValue = toJsValueImpl()
}
