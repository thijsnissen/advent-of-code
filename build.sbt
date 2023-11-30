ThisBuild / organization := "nl.thijsnissen"
ThisBuild / version      := Version.version
ThisBuild / scalaVersion := Version.scala

lazy val root =
  project
    .in(file("."))
    .settings(
      name           := "Advent of Code",
      normalizedName := "advent-of-code",
      description    := "My solutions to the Advent of Code puzzles",
    )
    .settings(commonSettings ++ commonImports ++ aliases)
    .aggregate(
      aoc2018,
      aoc2021,
      aoc2022,
      aoc2023,
      utilities,
    )

lazy val utilities =
  project
    .in(file("code/utilities"))
    .settings(libraryDependencies ++= Dependencies.common)
    .settings(commonSettings ++ commonImports)

lazy val aoc2018 =
  project
    .in(file("code/2018"))
    .dependsOn(utilities)
    .settings(libraryDependencies ++= Dependencies.common)
    .settings(commonSettings ++ commonImports)

lazy val aoc2021 =
  project
    .in(file("code/2021"))
    .dependsOn(utilities)
    .settings(libraryDependencies ++= Dependencies.common)
    .settings(commonSettings ++ commonImports)

lazy val aoc2022 =
  project
    .in(file("code/2022"))
    .dependsOn(utilities)
    .settings(libraryDependencies ++= Dependencies.common)
    .settings(commonSettings ++ commonImports)

lazy val aoc2023 =
  project
    .in(file("code/2023"))
    .dependsOn(utilities)
    .settings(libraryDependencies ++= Dependencies.common)
    .settings(commonSettings ++ commonImports)

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
    "-explain",
  ),
  Compile / run / fork         := true,
  Compile / run / connectInput := true,
  Compile / run / javaOptions += "-Xmx4G",
)

lazy val commonImports = Seq(
  scalacOptions +=
    Seq(
      "java.lang",
      "scala",
      "scala.Predef",
      "scala.annotation",
      "scala.util.chaining",
    ).mkString("-Yimports:", ",", "")
)

lazy val aliases =
  addCommandAlias("format", "scalafmtAll;scalafmtSbt")

ThisBuild / watchBeforeCommand := Watch.clearScreen
