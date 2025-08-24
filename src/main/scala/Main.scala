package com.github.aminmal

import scala.io.Source

import scajer.core.{ParseError, Parser, StrItr}
import scajer.json.JsValue

object Main extends App {
  val src             = Source.fromResource("input.json").mkString
  val Right(json)     = Parser.parse(src): @unchecked
  println(json.prettyPrint())
  val thirdHobbyTitle = (json.cursor ~> "hobbies" ~> 2 ~> "title").str
  println(s"third hobby's title is $thirdHobbyTitle")
}
