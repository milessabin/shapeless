val dottyLatestNightly = dottyLatestNightlyBuild.get
//val dottyVersion = dottyLatestNightly 
val dottyVersion = "3.0.0-RC2"

ThisBuild / organization := "org.typelevel"
ThisBuild / scalaVersion := dottyVersion
ThisBuild / crossScalaVersions := Seq(dottyVersion)
ThisBuild / updateOptions := updateOptions.value.withLatestSnapshots(false)

// GHA configuration

ThisBuild / githubWorkflowJavaVersions := Seq("adopt@1.8")

ThisBuild / githubWorkflowArtifactUpload := false

ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Sbt(List("validateJVM"), name = Some("Validate JVM"))
)

ThisBuild / githubWorkflowTargetTags ++= Seq("v*")
ThisBuild / githubWorkflowPublishTargetBranches :=
  Seq(RefPredicate.Equals(Ref.Branch("shapeless-3")), RefPredicate.StartsWith(Ref.Tag("v")))

ThisBuild / githubWorkflowPublishPreamble +=
  WorkflowStep.Use(UseRef.Public("olafurpg", "setup-gpg", "v3"))

ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    List("ci-release"),
    env = Map(
      "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
      "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
      "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
    )
  )
)

addCommandAlias("validateJVM", ";clean;compile;test")

lazy val modules: List[ProjectReference] = List(
  data,
  deriving,
  test,
  typeable
)

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-Xfatal-warnings",
    "-Yexplicit-nulls"
  ),
  sources in (Compile,doc) := Nil,

  libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
)

lazy val root = project.in(file("."))
  .aggregate(modules:_*)
  .settings(commonSettings:_*)
  .settings(noPublishSettings)

lazy val data = project
  .in(file("modules/data"))
  .settings(
    moduleName := "shapeless3-data"
  )
  .settings(commonSettings: _*)
  .settings(publishSettings)

lazy val deriving = project
  .in(file("modules/deriving"))
  .dependsOn(test % "test")
  .settings(
    moduleName := "shapeless3-deriving"
  )
  .settings(commonSettings: _*)
  .settings(publishSettings)

lazy val test = project
  .in(file("modules/test"))
  .settings(
    moduleName := "shapeless3-test"
  )
  .settings(commonSettings: _*)
  .settings(publishSettings)

lazy val typeable = project
  .in(file("modules/typeable"))
  .dependsOn(test % "test", data % "test")
  .settings(
    moduleName := "shapeless3-typeable"
  )
  .settings(commonSettings: _*)
  .settings(publishSettings)

lazy val local = project
  .in(file("local"))
  .dependsOn(data, deriving, test, typeable)
  .settings(
    moduleName := "shapeless3-local",
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
