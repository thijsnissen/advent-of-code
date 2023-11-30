import sbt.*

object Dependencies {
  lazy val common: Seq[ModuleID] = Seq(
    "org.scalatest" %% "scalatest" % Version.scalatest % "test",
    "com.lihaoyi"   %% "pprint"    % Version.pprint,
  )
}
