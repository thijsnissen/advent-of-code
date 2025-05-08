import sbt.*
import sbt.Keys.*

object Settings {
  lazy val common = Seq(
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

  lazy val imports = Seq(
    scalacOptions +=
      Seq(
        "java.lang",
        "scala",
        "scala.Predef",
        "scala.annotation",
        "scala.util.chaining"
      ).mkString("-Yimports:", ",", "")
  )
}
