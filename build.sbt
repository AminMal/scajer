ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.6"


lazy val root = (project in file("."))
  .settings(
    name := "Scajer",
    idePackagePrefix := Some("com.github.aminmal"),
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core"    % "0.14.6",
      "io.circe" %% "circe-parser"  % "0.14.6",
      "io.circe" %% "circe-generic" % "0.14.6",
      "com.typesafe.play" %% "play-json" % "2.10.7",
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"   % "2.37.4"
    ),
    scalafmtOnCompile := true
  )
