package com.github.aminmal

import scala.io.Source

import core.{Parser, StrItr}

object Main extends App {
  val src  = Source.fromResource("input.json").mkString
  val json = Parser.parse(src).toOption.get
  println(json.prettyPrint())

  val thirdHobbyTitle = (json.cursor ~> "hobbies" \~\ 2 ~> "title").value
  println(s"third hobby title is $thirdHobbyTitle")
}
