ThisBuild / organization := "nl.thijsnissen"
ThisBuild / version      := Version.app
ThisBuild / scalaVersion := Version.scala

lazy val root =
  project
    .in(file("."))
    .settings(name := "advent-of-code")
    .settings(description := "My solutions to the Advent of Code puzzles")
    .aggregate(utilities)
    .aggregate(aoc2018)
    .aggregate(aoc2022)
    .aggregate(aoc2023)

lazy val utilities =
  project
    .in(file("utilities"))
    .settings(libraryDependencies ++= Dependencies.common)

lazy val aoc2018 =
  project
    .in(file("2018"))
    .dependsOn(utilities)
    .settings(libraryDependencies ++= Dependencies.common)

lazy val aoc2022 =
  project
    .in(file("2022"))
    .dependsOn(utilities)
    .settings(libraryDependencies ++= Dependencies.common)

lazy val aoc2023 =
  project
    .in(file("2023"))
    .dependsOn(utilities)
    .settings(libraryDependencies ++= Dependencies.common)

ThisBuild / scalacOptions ++= Seq(
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
)

ThisBuild / watchBeforeCommand := Watch.clearScreen
Compile / run / fork           := true
Compile / run / connectInput   := true
Compile / run / javaOptions += "-Xmx4G"
