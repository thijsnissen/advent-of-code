ThisBuild / organization := "nl.thijsnissen"
ThisBuild / version      := Version.version
ThisBuild / scalaVersion := Version.scala

lazy val root =
  project
    .in(file("."))
    .settings(
      name           := "Advent of Code",
      normalizedName := "advent-of-code",
      description    := "My solutions to the Advent of Code puzzles"
    )
    .settings(Aliasses.common)
    .aggregate(
      aoc2018,
      aoc2021,
      aoc2022,
      aoc2023,
      utilities
    )

lazy val aoc2018 =
  project
    .in(file("code/2018"))
    .dependsOn(utilities)
    .settings(Settings.common ++ Settings.imports)
    .settings(libraryDependencies ++= Dependencies.common ++ Dependencies.test)

lazy val aoc2021 =
  project
    .in(file("code/2021"))
    .dependsOn(utilities)
    .settings(Settings.common ++ Settings.imports)
    .settings(libraryDependencies ++= Dependencies.common ++ Dependencies.test)

lazy val aoc2022 =
  project
    .in(file("code/2022"))
    .dependsOn(utilities)
    .settings(Settings.common ++ Settings.imports)
    .settings(libraryDependencies ++= Dependencies.common ++ Dependencies.test)

lazy val aoc2023 =
  project
    .in(file("code/2023"))
    .dependsOn(utilities)
    .settings(Settings.common ++ Settings.imports)
    .settings(libraryDependencies ++= Dependencies.common ++ Dependencies.test)

lazy val utilities =
  project
    .in(file("code/utilities"))
    .settings(Settings.common ++ Settings.imports)
    .settings(libraryDependencies ++= Dependencies.common ++ Dependencies.test)

ThisBuild / watchBeforeCommand := Watch.clearScreen
