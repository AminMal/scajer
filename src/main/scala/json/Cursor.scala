package com.github.aminmal
package json

import scala.annotation.targetName
import scala.util.{Failure, Success, Try}

import json.JsValue.*

class Cursor private[json] (json: JsValue, error: Option[CursorError], cpath: String) {

  @targetName("atPath")
  def ~>(path: String): Cursor = (json, error) match {
    case (_, Some(er))            => this
    case (JsObject(keyValues), _) =>
      val newPath = s"$cpath.$path"
      keyValues.find((key, _) => key == path).map(_._2) match {
        case Some(v) => Cursor(v, None, newPath)
        case None    =>
          val err = CursorError.PathDoesNotExist(newPath)
          Cursor(JsNull, Some(err), newPath)
      }
    case _                        =>
      Cursor(JsNull, Some(CursorError.NotAnObject(cpath)), cpath)
  }

  @targetName("atIndex")
  def \~\(idx: Int): Cursor = (json, error) match {
    case (_, Some(err))    => this
    case (JsArray(arr), _) =>
      val newPath = s"$cpath.$idx"
      Try(arr(idx)) match {
        case Failure(_) => Cursor(JsNull, Some(CursorError.IndexOutOfBounds(newPath, arr.size)), newPath)
        case Success(v) => Cursor(v, None, newPath)
      }
    case (other, _)        =>
      Cursor(JsNull, Some(CursorError.NotAnArray(cpath)), cpath)
  }

  def value: CursorError | JsValue = error match {
    case Some(err) => err
    case _         => json
  }
}

object Cursor {
  private[json] def from(json: JsValue): Cursor = Cursor(json, None, "")
}
