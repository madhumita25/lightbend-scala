import sbt._

object Version {
  val scalaVer     = "2.12.8"
  val scalaTest    = "3.0.6"
  val playJson     = "2.7.0"
}

object Library {
  val scalaTest    = "org.scalatest"          %% "scalatest"                % Version.scalaTest
  val playJson     = "com.typesafe.play"      %% "play-json"                % Version.playJson
}

object Dependencies {

  import Library._

  val dependencies = List(
    playJson,
    scalaTest % "test"
  )
}
