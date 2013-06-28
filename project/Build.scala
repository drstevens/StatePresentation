import sbt._
import sbt.Keys._

object StateexampleBuild extends Build {

  lazy val stateexample = Project(
    id = "stateexample",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "StateExample",
      organization := "org.example",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.10.1"
      // add other settings here
    )
  )
}
