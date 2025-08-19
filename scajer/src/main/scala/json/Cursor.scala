package json

import scala.annotation.targetName
import scala.util.{Failure, Success, Try}

import json.JsValue.*

class Cursor private[json] (jsValue: JsValue, error: Option[CursorError], cpath: String) {

  @targetName("atPath")
  def ~>(path: String): Cursor = (jsValue, error) match {
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
  def ~>>(idx: Int): Cursor = (jsValue, error) match {
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
    case _         => jsValue
  }

  def str: CursorError | String = error match {
    case Some(err) => err
    case _         =>
      jsValue match {
        case JsString(s) => s
        case other       => CursorError.IncompatibleValueType("String", other.tpe, cpath)
      }
  }

  def strOpt: Option[String] = str match {
    case s: String => Some(s)
    case _         => None
  }

  def getStr: String = str match {
    case e: CursorError => throw e.toJsonException
    case s: String      => s
  }

  def long: CursorError | Long = error match {
    case Some(err) => err
    case _         =>
      jsValue match {
        case JsNumber(n: Num.L) => n.n
        case other              => CursorError.IncompatibleValueType("Long", other.tpe, cpath)
      }
  }

  def longOpt: Option[Long] = long match {
    case l: Long => Some(l)
    case _       => None
  }

  def getLong: Long = long match {
    case e: CursorError => throw e.toJsonException
    case l: Long        => l
  }

  def double: CursorError | Double = error match {
    case Some(err) => err
    case _         =>
      jsValue match {
        case JsNumber(n: Num.D) => n.n
        case other              => CursorError.IncompatibleValueType("Double", other.tpe, cpath)
      }
  }

  def doubleOpt: Option[Double] = double match {
    case l: Double => Some(l)
    case _         => None
  }

  def getDouble: Double = double match {
    case e: CursorError => throw e.toJsonException
    case d: Double      => d
  }

  def bool: CursorError | Boolean = error match {
    case Some(err) => err
    case _         =>
      jsValue match {
        case JsBool(b) => b
        case other     => CursorError.IncompatibleValueType("Boolean", other.tpe, cpath)
      }
  }

  def boolOpt: Option[Boolean] = bool match {
    case b: Boolean => Some(b)
    case _          => None
  }

  def getBool: Boolean = bool match {
    case e: CursorError => throw e.toJsonException
    case b: Boolean     => b
  }
}

object Cursor {
  private[json] def from(json: JsValue): Cursor = Cursor(json, None, "")
}
