package com.github.aminmal
package json

import scala.collection.mutable

enum JsValue {
  case JsNull
  case JsString(s: String)
  case JsNumber(n: Num)
  case JsBool(b: Boolean)
  case JsObject(underlying: mutable.Map[String, JsValue])
  case JsArray(arr: mutable.ListBuffer[JsValue])

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
