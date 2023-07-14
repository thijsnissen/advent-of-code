ThisBuild / name         := "advent-of-code"
ThisBuild / version      := "0.1.0"
ThisBuild / scalaVersion := "3.3.0"

ThisBuild / libraryDependencies ++= Seq(
	"org.scalatest"  %% "scalatest"  % "3.2.16" % "test",
	"com.lihaoyi" %% "pprint" % "0.8.1"
)

lazy val utilities =
	project
		.in(file("utilities"))

lazy val aoc2018 =
	project
		.in(file("2018"))
		.dependsOn(utilities)

lazy val aoc2022 =
	project
		.in(file("2022"))
		.dependsOn(utilities)

scalacOptions ++= Seq(
	"-encoding", "utf8",
	"-feature",
	"-language:implicitConversions",
	"-language:existentials",
	"-unchecked",
	"-Werror",
	"-deprecation"
)

Compile / run / fork := true
Compile / run / connectInput := true
Compile / run / javaOptions += "-Xmx4G"

ThisBuild / watchBeforeCommand := Watch.clearScreen

ThisBuild / shellPrompt := {
	(state: State) =>
		s"sbt:${(ThisBuild / name).value}:" +
			s"${Project.extract(state).currentProject.id}" +
			s"${scala.Console.CYAN}>${scala.Console.RESET}"
}
