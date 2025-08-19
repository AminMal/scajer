package bench

import java.nio.file.{Files, Paths}
import java.util.concurrent.TimeUnit

import scala.concurrent.duration.*
import scala.io.Source

import core.{Indexer as MyIndexer, Parser as MyParser}
import io.circe.parser.*
import org.openjdk.jmh.annotations.*
import play.api.libs.json.*

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput, Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class ParsingBenchmark {

  var src: String = _

  // initialize test JSON (say medium.json in resources)
  @Setup(Level.Trial)
  def setup(): Unit =
    src = Source.fromResource("medium_input.json").mkString

  // --- Circe ---
  @Benchmark
  def circeParse(): Either[io.circe.ParsingFailure, io.circe.Json] =
    parse(src)

  // --- Play JSON ---
  @Benchmark
  def playParse(): JsValue =
    Json.parse(src)

  // --- My Parser ---
  @Benchmark
  def myParserParse(): Either[core.ParseError, json.JsValue] =
    MyParser.parse(src)

  // --- My Indexer ---
  @Benchmark
  def myIndexerParse(): Either[core.ParseError, json.IndexedJsValue] =
    MyIndexer.parse(src)
}
