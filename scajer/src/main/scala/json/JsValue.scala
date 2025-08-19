package json

import scala.collection.mutable

enum JsValue {
  case JsNull
  case JsString(s: String)
  case JsNumber(n: Num)
  case JsBool(b: Boolean)
  case JsObject(underlying: mutable.Map[String, JsValue])
  case JsArray(arr: mutable.ListBuffer[JsValue])

  private[json] def tpe: String = this match
    case JsValue.JsNull               => "NULL"
    case JsValue.JsString(s)          => "String"
    case JsValue.JsNumber(_: Num.D)   => "Double"
    case JsValue.JsNumber(_: Num.L)   => "Long"
    case JsValue.JsBool(b)            => "Boolean"
    case JsValue.JsObject(underlying) => "Object"
    case JsValue.JsArray(arr)         => "Array"

  private def indentImpl(indentStr: String, level: Int): String = {
    val spaceInner = indentStr.repeat(level)
    val spaceOuter = indentStr.repeat(level - 1)

    this match {
      case JsValue.JsNull =>
        "null"

      case JsValue.JsString(s) =>
        s"\"$s\""

      case JsValue.JsNumber(Num.L(l)) =>
        l.toString

      case JsValue.JsNumber(Num.D(d)) =>
        d.toString

      case JsValue.JsBool(b) =>
        b.toString

      case JsValue.JsObject(obj) =>
        if obj.isEmpty then "{}"
        else
          val inner =
            obj
              .map { case (k, v) =>
                s"\n$spaceInner\"$k\": ${v.indentImpl(indentStr, level + 1)}"
              }
              .mkString(",")
          s"{$inner\n$spaceOuter}"

      case JsValue.JsArray(arr) =>
        if arr.isEmpty then "[]"
        else
          val inner =
            arr
              .map { v =>
                s"\n$spaceInner${v.indentImpl(indentStr, level + 1)}"
              }
              .mkString(",")
          s"[$inner\n$spaceOuter]"
    }
  }

  def prettyPrint(): String = indentImpl(" ".repeat(2), 1)

  def indent(s: String): String = indentImpl(s, 1)

  def cursor: Cursor = Cursor.from(this)
}
