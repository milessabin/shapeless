val dottyLatestNightly = dottyLatestNightlyBuild.get
//val dottyVersion = dottyLatestNightly 
val dottyVersion = "0.24.0-bin-20200409-f64e879-NIGHTLY"
//val dottyVersion = "0.23.0-RC1"
val scala2Version = "2.13.1"

addCommandAlias("root", ";project root")
addCommandAlias("core", ";project core")

addCommandAlias("validate", ";root;core/compile;core/test") // no ;core/doc due to dottydoc crash

inThisBuild(Seq(
  organization := "org.typelevel",
  scalaVersion := dottyVersion,
  //crossScalaVersions := Seq(dottyVersion, dottyLatestNightly, scala2Version),
  updateOptions := updateOptions.value.withLatestSnapshots(false),
))

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-Xfatal-warnings",
    "-Yexplicit-nulls"
  ),

  libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
)

lazy val root = project.in(file("."))
  .aggregate(core)
  .dependsOn(core)
  .settings(commonSettings:_*)
  .settings(noPublishSettings)

lazy val core = project
  .in(file("core"))
  .settings(
    moduleName := "shapeless-core",
    sources in (Compile,doc) := Nil
  )
  .settings(commonSettings: _*)
  .settings(publishSettings)

lazy val local = project
  .in(file("local"))
  .dependsOn(core)
  .settings(
    moduleName := "shapeless-local",
    scalacOptions ++= List("-Xmax-inlines", "1000"),
    //scalacOptions += "-Xprint:posttyper",
    scalacOptions in console in Compile -= "-Xprint:posttyper",
    initialCommands in console := """import shapeless._ ; import scala.deriving._"""
  )
  .settings(commonSettings: _*)
  .settings(noPublishSettings)

lazy val publishSettings = Seq(
  publishArtifact in Test := false,
  pomIncludeRepository := (_ => false),
  homepage := Some(url("https://github.com/milessabin/shapeless")),
  licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  scmInfo := Some(ScmInfo(url("https://github.com/milessabin/shapeless"), "scm:git:git@github.com:milessabin/shapeless.git")),
  developers := List(
    Developer("milessabin", "Miles Sabin", "miles@milessabin.com", url("http://milessabin.com/blog")),
    Developer("joroKr21", "Georgi Krastev", "joro.kr.21@gmail.com", url("https://twitter.com/Joro_Kr"))
  )
)

lazy val noPublishSettings =
  skip in publish := true
