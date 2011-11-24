name := "rank-n-poly"

version := "1.0"

scalaVersion := "2.10.0-SNAPSHOT"

scalacOptions ++= Seq("-unchecked", "-deprecation")

resolvers ++= Seq(
  "scala-tools.org snapshots" at "http://scala-tools.org/repo-snapshots",
  "scala-tools.org releases"  at "http://scala-tools.org/repo-releases"
)

libraryDependencies ++= Seq(
  "com.novocode" % "junit-interface" % "0.7" % "test->default"
)
