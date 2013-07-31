Revolver.settings

resolvers += "Typesafe Snapshots" at "http://repo.typesafe.com/typesafe/snapshots/"

scalacOptions ++= Seq("-language:postfixOps", "-language:implicitConversions", "-feature")
 
libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.0.2",
  "org.specs2" %% "specs2" % "2.1" % "test"
)
 
initialCommands in console := "import scalaz._, Scalaz._, org.example._, Demo._, JobExamples._"
