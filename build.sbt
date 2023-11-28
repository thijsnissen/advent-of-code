ThisBuild / organization := "nl.thijsnissen"
ThisBuild / version      := Version.version
ThisBuild / scalaVersion := Version.scala

lazy val root =
  project
    .in(file("."))
    .settings(name := "advent-of-code")
    .settings(description := "My solutions to the Advent of Code puzzles")
    .settings(commonSettings)
    .settings(aliases)
    .aggregate(
      aoc2018,
      aoc2022,
      aoc2023,
      utilities
    )

lazy val utilities =
  project
    .in(file("code/utilities"))
    .settings(libraryDependencies ++= Dependencies.common)
    .settings(commonSettings)

lazy val aoc2018 =
  project
    .in(file("code/2018"))
    .dependsOn(utilities)
    .settings(libraryDependencies ++= Dependencies.common)
    .settings(commonSettings)

lazy val aoc2022 =
  project
    .in(file("code/2022"))
    .dependsOn(utilities)
    .settings(libraryDependencies ++= Dependencies.common)
    .settings(commonSettings)

lazy val aoc2023 =
  project
    .in(file("code/2023"))
    .dependsOn(utilities)
    .settings(libraryDependencies ++= Dependencies.common)
    .settings(commonSettings)

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-encoding",
    "utf8",
    "-language:implicitConversions",
    "-language:existentials",
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Werror",
    "-Wunused:imports",
    "-Wunused:locals",
    "-print-lines",
    "-explain"
  ),
  Compile / run / fork         := true,
  Compile / run / connectInput := true,
  Compile / run / javaOptions += "-Xmx4G"
)

lazy val aliases = {
  addCommandAlias("format", "scalafmtAll;scalafmtSbt")
}

ThisBuild / watchBeforeCommand := Watch.clearScreen
