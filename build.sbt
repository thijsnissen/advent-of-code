name    := "advent-of-code"
version := "0.1.0"

ThisBuild / scalaVersion := "3.3.0"
ThisBuild / libraryDependencies ++= Seq(
	"org.scalatest"  %% "scalatest"  % "3.2.15" % "test",
	"com.lihaoyi" %% "pprint" % "0.8.1"
)

lazy val aoc2018 = project.in(file("2018"))
lazy val aoc2022 = project.in(file("2022"))

scalacOptions ++= Seq(
	"-encoding", "utf8",
	"-feature",
	"-language:implicitConversions",
	"-language:existentials",
	"-unchecked",
	"-Werror",
	"-deprecation",
	"-Wunused:all"
)

Compile / run / fork := true
Compile / run / javaOptions += "-Xmx4G"
