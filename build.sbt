name := "scala-experiments"

version := "0.1"

scalaVersion := "2.12.4"

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.8.2"
libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.5" % "test"