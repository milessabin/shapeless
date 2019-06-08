//val dottyVersion = dottyLatestNightlyBuild.get
val dottyVersion = "0.16.0-bin-20190606-c46553a-NIGHTLY"
val scala2Version = "2.13.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "shapeless",
    organization := "org.typelevel",

    scalaVersion := dottyVersion,
    crossScalaVersions := Seq(dottyVersion, scala2Version),

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
