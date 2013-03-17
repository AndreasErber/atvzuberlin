import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "atvzuberlin"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
    jdbc,
    anorm,
    "com.typesafe.slick" %% "slick" % "1.0.0",
    "com.h2database" % "h2" % "1.3.166",
    "org.scalaz" %% "scalaz-core" % "6.0.4",
    "org.scalatest" % "scalatest_2.10" % "2.0.M6-SNAP8",
    "org.scala-lang" % "scala-actors" % "2.10.0",
    "org.scalaz" % "scalaz-core_2.10" % "6.0.4",
    "postgresql" % "postgresql" % "9.1-901.jdbc4"
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here      
    testOptions in Test := Nil
  )

}
