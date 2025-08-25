
ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.6"

lazy val scajer = (project in file("scajer"))
  .settings(
    name := "Scajer",
    idePackagePrefix := Some("scajer"),
    scalafmtOnCompile := true,
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.1"    % Test
  )

lazy val benchmark = (project in file("bench"))
  .enablePlugins(JmhPlugin)
  .settings(
    name := "Bench",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core"    % "0.14.14",
      "io.circe" %% "circe-parser"  % "0.14.14",
      "io.circe" %% "circe-generic" % "0.14.14",
      "com.typesafe.play" %% "play-json" % "2.10.7"
    ),
    scalafmtOnCompile := true
  )
  .dependsOn(scajer)

lazy val root = (project in file("."))
  .settings(
    name := "Scajer",
    idePackagePrefix := Some("com.github.aminmal"),
    scalafmtOnCompile := true
  )
  .dependsOn(scajer)
