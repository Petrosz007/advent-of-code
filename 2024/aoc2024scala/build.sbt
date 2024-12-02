scalacOptions ++= Seq(
  "-experimental",
  "-language:experimental.namedTuples"
)

ThisBuild / scalaVersion := "3.5.2"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)
