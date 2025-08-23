package com.github.aminmal

import scala.io.Source

import core.{ParseError, Parser, StrItr}
import json.JsValue

object Main extends App {
  val src = Source.fromResource("input.json").mkString
  println(Parser.parse(src).toOption.get.prettyPrint())
}
