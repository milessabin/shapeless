//val dottyVersion = dottyLatestNightlyBuild.get
//val dottyVersion = "0.19.0-bin-20190915-6f6d973-NIGHTLY"
val dottyVersion = "0.19.0-RC1"
val scala2Version = "2.13.0"

inThisBuild(Seq(
  organization := "org.typelevel",
  scalaVersion := dottyVersion,
  crossScalaVersions := Seq(dottyVersion, scala2Version),
  updateOptions := updateOptions.value.withLatestSnapshots(false)
))

lazy val commonSettings = Seq(
  scalaVersion := dottyVersion,
  crossScalaVersions := Seq(dottyVersion, scala2Version),

  libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
)

lazy val noPublishSettings =
  skip in publish := true

lazy val root = project.in(file("."))
  .aggregate(core)
  .dependsOn(core)
  .settings(commonSettings:_*)
  .settings(noPublishSettings)

lazy val core = project
  .in(file("core"))
  .settings(
    moduleName := "shapeless-core",
  )
  .settings(commonSettings: _*)

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
