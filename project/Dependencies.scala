import sbt.*

object Dependencies {
  lazy val common = Seq(
    "com.lihaoyi" %% "pprint" % Version.pprint
  )

  lazy val test = Seq(
    "org.scalatest" %% "scalatest" % Version.scalatest % "test"
  )
}
