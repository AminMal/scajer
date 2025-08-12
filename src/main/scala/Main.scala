package com.github.aminmal

import scala.io.Source

import core.{Parser, StrItr}

object Main extends App {
  val src = Source.fromResource("input.json").mkString

  def bench[T](n: Int, op: () => T)(tag: String): Long = {
    val start = System.currentTimeMillis()
    (0 until n).foreach(_ => op())
    val end   = System.currentTimeMillis()
    end - start
  }

  val mine   = bench(70000, () => Parser.parse(src))("my_parser")
  val circes = bench(70000, () => io.circe.parser.parse(src))("circe")
  val plays  = bench(70000, () => play.api.libs.json.Json.parse(src))("play")

  println(s"mine took $mine ms, circe took $circes ms, play took $plays ms")
//  println(Parser.parse(src).toOption.get.prettyPrint())
}
