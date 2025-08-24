# Scajer

Practice project: A high-performance, no dependency JSON parser for Scala.

## Benchmarks

The following results were measured with JMH on a modern JVM.  
Comparison includes **Circe**, **Play JSON**, and **Scajer** for medium and large (raw String) inputs:
```
[info] Benchmark                           Mode  Cnt   Score    Error   Units
[info] ParsingBenchmark.circeLargeParse   thrpt    3   1,764 ±  0,073  ops/ms
[info] ParsingBenchmark.circeParse        thrpt    3  53,754 ±  0,081  ops/ms
[info] ParsingBenchmark.playLargeParse    thrpt    3   0,742 ±  0,027  ops/ms
[info] ParsingBenchmark.playParse         thrpt    3  36,129 ±  1,231  ops/ms
[info] ParsingBenchmark.scajerLargeParse  thrpt    3   1,630 ±  0,048  ops/ms
[info] ParsingBenchmark.scajerParse       thrpt    3  63,402 ±  0,964  ops/ms
[info] ParsingBenchmark.circeLargeParse    avgt    3   0,623 ±  0,031   ms/op
[info] ParsingBenchmark.circeParse         avgt    3   0,018 ±  0,001   ms/op
[info] ParsingBenchmark.playLargeParse     avgt    3   1,374 ±  0,030   ms/op
[info] ParsingBenchmark.playParse          avgt    3   0,026 ±  0,001   ms/op
[info] ParsingBenchmark.scajerLargeParse   avgt    3   0,594 ±  0,020   ms/op
[info] ParsingBenchmark.scajerParse        avgt    3   0,017 ±  0,001   ms/op
```