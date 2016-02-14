import com.typesafe.sbt.pgp.PgpKeys.publishSigned
import org.scalajs.sbtplugin.cross.{ CrossProject, CrossType }
import ReleaseTransformations._

import sbtbuildinfo.Plugin.{ buildInfoSettings => defaultBuildInfoSettings, _ }

import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import com.typesafe.tools.mima.plugin.MimaKeys
import MimaKeys.{ previousArtifacts, binaryIssueFilters }

import com.typesafe.sbt.osgi.SbtOsgi.{ osgiSettings => defaultOsgiSettings, _ }

import com.typesafe.sbt.SbtGit._
import GitKeys._

lazy val buildSettings = Seq(
  organization := "com.chuusai",
  scalaVersion := "2.11.7",
  crossScalaVersions := Seq("2.10.6", "2.11.7", "2.12.0-M3")
)

addCommandAlias("root", ";project root")
addCommandAlias("core", ";project coreJVM")
addCommandAlias("scratch", ";project scratchJVM")
addCommandAlias("examples", ";project examplesJVM")

addCommandAlias("validate", ";root;compile;test")
addCommandAlias("validateJVM", ";coreJVM/compile;coreJVM/mimaReportBinaryIssues;coreJVM/test;examplesJVM/compile;coreJVM/doc")
addCommandAlias("validateJS", ";coreJS/compile;coreJS/mimaReportBinaryIssues;coreJS/test;examplesJS/compile;coreJS/doc")

addCommandAlias("runAll", ";examplesJVM/runAll")
addCommandAlias("releaseAll", ";root;release")

lazy val commonSettings = Seq(
  scalacOptions := Seq(
    "-feature",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-Xfatal-warnings",
    "-deprecation",
    "-unchecked"
  ),

  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),

  scalacOptions in console in Compile -= "-Xfatal-warnings",
  scalacOptions in console in Test    -= "-Xfatal-warnings",

  initialCommands in console := """import shapeless._""",

  scmInfo :=
    Some(ScmInfo(
      url("https://github.com/milessabin/shapeless"),
      "scm:git:git@github.com:milessabin/shapeless.git"
    ))
) ++ crossVersionSharedSources ++ scalaMacroDependencies

def configureJUnit(crossProject: CrossProject) = {
  crossProject
  .jsConfigure(_.enablePlugins(ScalaJSJUnitPlugin))
  .jvmSettings(
    libraryDependencies +=
      "com.novocode" % "junit-interface" % "0.9" % "test"
  )
}

lazy val commonJsSettings = Seq(
  scalaJSUseRhino in Global := false,
  parallelExecution in Test := false
)

lazy val commonJvmSettings = Seq(
  parallelExecution in Test := false
)

lazy val coreSettings = buildSettings ++ commonSettings ++ publishSettings

lazy val root = project.in(file("."))
  .aggregate(coreJS, coreJVM)
  .dependsOn(coreJS, coreJVM)
  .settings(coreSettings:_*)
  .settings(noPublishSettings)

lazy val core = crossProject.crossType(CrossType.Full)
  .configure(configureJUnit)
  .settings(moduleName := "shapeless")
  .settings(coreSettings:_*)
  .settings(buildInfoSettings:_*)
  .settings(osgiSettings:_*)
  .settings(
    sourceGenerators in Compile <+= (sourceManaged in Compile).map(Boilerplate.gen),
    sourceGenerators in Compile <+= buildInfo,

    mappings in (Compile, packageSrc) <++=
      (sourceManaged in Compile, managedSources in Compile) map { (base, srcs) =>
        (srcs pair (Path.rebase(base/"sbt-buildinfo", "shapeless") | Path.relativeTo(base) | Path.flat))
      }
  )
  .settings(mimaSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val scratch = crossProject.crossType(CrossType.Pure)
  .configure(configureJUnit)
  .dependsOn(core)
  .settings(moduleName := "scratch")
  .settings(coreSettings:_*)
  .settings(noPublishSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

lazy val scratchJVM = scratch.jvm
lazy val scratchJS = scratch.js

lazy val runAll = TaskKey[Unit]("runAll")

def runAllIn(config: Configuration) = {
  runAll in config <<= (discoveredMainClasses in config, runner in run, fullClasspath in config, streams) map {
    (classes, runner, cp, s) => classes.foreach(c => runner.run(c, Attributed.data(cp), Seq(), s.log))
  }
}

lazy val examples = crossProject.crossType(CrossType.Pure)
  .configure(configureJUnit)
  .dependsOn(core)
  .settings(moduleName := "examples")
  .settings(runAllIn(Compile))
  .settings(coreSettings:_*)
  .settings(noPublishSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

lazy val examplesJVM = examples.jvm
lazy val examplesJS = examples.js

lazy val scalaMacroDependencies: Seq[Setting[_]] = Seq(
  libraryDependencies ++= Seq(
    "org.typelevel" %% "macro-compat" % "1.1.1-SNAPSHOT",
    "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
    "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  ),
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      // if scala 2.11+ is used, quasiquotes are merged into scala-reflect
      case Some((2, scalaMajor)) if scalaMajor >= 11 => Seq()
      // in Scala 2.10, quasiquotes are provided by macro paradise
      case Some((2, 10)) =>
        Seq("org.scalamacros" %% "quasiquotes" % "2.1.0" cross CrossVersion.binary)
    }
  }
)

lazy val crossVersionSharedSources: Seq[Setting[_]] =
  Seq(Compile, Test).map { sc =>
    (unmanagedSourceDirectories in sc) ++= {
      (unmanagedSourceDirectories in sc ).value.flatMap { dir: File =>
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, y)) if y == 10 => List(new File(dir.getPath + "_2.10"))
          case Some((2, y)) if y >= 11 => List(new File(dir.getPath + "_2.11+"))
          case Some((2, y)) if y >= 12 => List(new File(dir.getPath + "_2.11+"), new File(dir.getPath + "_2.12+"))
        }
      }
    }
  }

lazy val publishSettings = Seq(
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  publishTo <<= version { (v: String) =>
    val nexus = "https://oss.sonatype.org/"
    if (v.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  homepage := Some(url("https://github.com/milessabin/shapeless")),
  licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  scmInfo := Some(ScmInfo(url("https://github.com/milessabin/shapeless"), "scm:git:git@github.com:milessabin/shapeless.git")),
  pomExtra := (
    <developers>
      <developer>
        <id>milessabin</id>
        <name>Miles Sabin</name>
        <url>http://milessabin.com/blog</url>
      </developer>
    </developers>
  )
)

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

lazy val mimaSettings = mimaDefaultSettings ++ Seq(
  previousArtifacts := Set(), // Set(organization.value %% moduleName.value % "2.3.0"),

  binaryIssueFilters ++= {
    import com.typesafe.tools.mima.core._
    import com.typesafe.tools.mima.core.ProblemFilters._

    // Filtering the methods that were added since the checked version
    // (these only break forward compatibility, not the backward one)
    Seq(
    )
  }
)

lazy val buildInfoSettings = defaultBuildInfoSettings ++ Seq(
  buildInfoPackage := "shapeless",
  buildInfoKeys := Seq[BuildInfoKey](version, scalaVersion),
  buildInfoKeys ++= Seq[BuildInfoKey](
    version,
    scalaVersion,
    gitHeadCommit,
    BuildInfoKey.action("buildTime") {
      System.currentTimeMillis
    }
  )
)

lazy val osgiSettings = defaultOsgiSettings ++ Seq(
  OsgiKeys.bundleSymbolicName := "shapeless",
  OsgiKeys.exportPackage := Seq("shapeless.*;version=${Bundle-Version}"),
  OsgiKeys.importPackage := Seq("""!scala.quasiquotes,scala.*;version="$<range;[==,=+);$<@>>""""),
  OsgiKeys.additionalHeaders := Map("-removeheaders" -> "Include-Resource,Private-Package")
)

lazy val sharedReleaseProcess = Seq(
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    publishArtifacts,
    setNextVersion,
    commitNextVersion,
    ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
    pushChanges
  )
)

credentials ++= (for {
  user <- sys.env.get("SONATYPE_USER")
  pass <- sys.env.get("SONATYPE_PASS")
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", user, pass)).toSeq
