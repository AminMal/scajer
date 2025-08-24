package bench

import java.util.concurrent.TimeUnit

import scala.io.Source

import core.Parser as MyParser
import io.circe.parser.*
import org.openjdk.jmh.annotations.*
import play.api.libs.json.*

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput, Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class ParsingBenchmark {

  var src: String      = _
  var largeSrc: String = _

  // initialize test JSON (say medium.json in resources)
  @Setup(Level.Trial)
  def setup(): Unit                                        =
    src = Source.fromResource("medium_input.json").mkString
    largeSrc = Source.fromResource("large_input.json").mkString

    // --- Scajer ---
  @Benchmark
  def scajerParse(): Either[core.ParseError, json.JsValue] =
    MyParser.parse(src)

  @Benchmark
  def scajerLargeParse(): Either[core.ParseError, json.JsValue] =
    MyParser.parse(largeSrc)

  // --- Circe ---
  @Benchmark
  def circeParse(): Either[io.circe.ParsingFailure, io.circe.Json] =
    parse(src)

  @Benchmark
  def circeLargeParse(): Either[io.circe.ParsingFailure, io.circe.Json] =
    parse(largeSrc)

  // --- Play JSON ---
  @Benchmark
  def playParse(): JsValue =
    Json.parse(src)

  @Benchmark
  def playLargeParse(): JsValue =
    Json.parse(largeSrc)

}
