package com.github.aminmal

import scala.io.Source

import core.{Indexer, ParseError, Parser, StrItr}
import json.JsValue

object Main extends App {
  val src = Source.fromResource("large_input.json").mkString

  def bench[T](n: Int, op: () => T)(tag: String): (Long, String) = {
    val start = System.currentTimeMillis()
    (0 until n).foreach(_ => op())
    val end   = System.currentTimeMillis()
    (end - start) -> tag
  }

  val n                      = 10
  val myParser               = bench(n, () => Parser.parse(src))("my_parser")
  val myIndexer              = bench(n, () => Indexer.parse(src))("my_indexer")
  val myIndexerPlusConverter = bench(n, () => Indexer.parse(src).map(_.toJsValue))("my_indexer_plus_converter")

  List(
    myParser,
    myIndexer,
    myIndexerPlusConverter
  ).foreach((dur, tag) => println(s"$tag took $dur ms to process $n times the file"))

//  println(Parser.parse(src).toOption.get.prettyPrint())
}
