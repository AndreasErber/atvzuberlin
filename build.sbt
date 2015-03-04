scalaVersion := "2.10.4"

name := "atvzuberlin"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  "com.typesafe.slick" %% "slick" % "1.0.0",
  "com.h2database" % "h2" % "1.4.184",
  "org.scalatest" %% "scalatest" % "2.2.3",
  "org.scala-lang" % "scala-actors" % "2.10.4",
  "org.scalaz" %% "scalaz-core" % "7.1.0",
  "postgresql" % "postgresql" % "9.1-901.jdbc4",
  "com.typesafe.play" %% "play-mailer" % "2.4.0"
)

lazy val root = (project in file(".")).enablePlugins(PlayScala)
