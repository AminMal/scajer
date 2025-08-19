package json

import scala.collection.mutable

enum JsPointer {
  case True, False, Null
  case Str(s: String)
  case N(start: Int, end: Int)
  case Obj(keyValues: mutable.Map[String, JsPointer])
  case Arr(values: mutable.ListBuffer[JsPointer])
}
