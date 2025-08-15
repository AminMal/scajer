package com.github.aminmal

import scala.io.Source

import core.{Parser, StrItr}

object Main extends App {
  val src = Source.fromResource("input.json").mkString
  println(Parser.parse(src).toOption.get.prettyPrint())
}
