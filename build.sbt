import com.typesafe.sbt.pgp.PgpKeys.publishSigned
import org.scalajs.sbtplugin.ScalaJSCrossVersion
import org.scalajs.sbtplugin.cross.{ CrossProject, CrossType }
import ReleaseTransformations._

import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import com.typesafe.tools.mima.plugin.MimaKeys
import MimaKeys.{ previousArtifacts, binaryIssueFilters }

import com.typesafe.sbt.osgi.SbtOsgi.{ osgiSettings => defaultOsgiSettings, _ }

import com.typesafe.sbt.SbtGit._
import GitKeys._

lazy val scoverageSettings = Seq(
  coverageMinimum := 60,
  coverageFailOnMinimum := false,
  coverageExcludedFiles := ".*/src/test/.*"
)

lazy val buildSettings = Seq(
  organization := "com.chuusai",
  scalaVersion := "2.11.8",
  crossScalaVersions := Seq("2.10.6", "2.11.8", "2.12.0-M5")
)

addCommandAlias("root", ";project root")
addCommandAlias("core", ";project coreJVM")
addCommandAlias("scratch", ";project scratchJVM")
addCommandAlias("examples", ";project examplesJVM")

addCommandAlias("validate", ";root;validateJVM;validateJS")
addCommandAlias("validateJVM", ";coreJVM/compile;coreJVM/mimaReportBinaryIssues;coreJVM/test;examplesJVM/compile;coreJVM/doc")
addCommandAlias("validateJS", ";coreJS/compile;coreJS/mimaReportBinaryIssues;coreJS/test;examplesJS/compile;coreJS/doc")

addCommandAlias("runAll", ";examplesJVM/runAll")
addCommandAlias("releaseAll", ";root;release skip-tests")

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
  .settings(
    /* The `test-plugin` configuration adds a plugin only to the `test`
     * configuration. It is a refinement of the `plugin` configuration which adds
     * it to both `compile` and `test`.
     */
    ivyConfigurations += config("test-plugin").hide,
    libraryDependencies ++= {
      if (scalaVersion.value.startsWith("2.12."))
        Seq("org.scala-js" % "scala-junit-mixin-plugin" % "0.1.0" % "test-plugin" cross CrossVersion.full)
      else
        Seq.empty
    },
    scalacOptions in Test ++= {
      val report = update.value
      val jars = report.select(configurationFilter("test-plugin"))
      for {
        jar <- jars
        jarPath = jar.getPath
        // This is a hack to filter out the dependencies of the plugins
        if jarPath.contains("plugin")
      } yield {
        s"-Xplugin:$jarPath"
      }
    }
  )
}

val cmdlineProfile = sys.props.getOrElse("sbt.profile", default = "")

def profile(crossProject: CrossProject) = cmdlineProfile match {
  case "2.12.x" =>
    crossProject
      .jsConfigure(_.disablePlugins(scoverage.ScoverageSbtPlugin))
      .jvmConfigure(_.disablePlugins(scoverage.ScoverageSbtPlugin))

  case _ => crossProject
}

def profile: Project ⇒ Project = p => cmdlineProfile match {
  case "2.12.x" => p.disablePlugins(scoverage.ScoverageSbtPlugin)
  case _ => p
}

lazy val commonJsSettings = Seq(
  scalacOptions += {
    val tagOrHash =
      if(isSnapshot.value) sys.process.Process("git rev-parse HEAD").lines_!.head
      else tagName.value
    val a = (baseDirectory in LocalRootProject).value.toURI.toString
    val g = "https://raw.githubusercontent.com/milessabin/shapeless/" + tagOrHash
    s"-P:scalajs:mapSourceURI:$a->$g/"
  },
  scalaJSUseRhino in Global := false,
  parallelExecution in Test := false,
  coverageExcludedPackages := ".*"
)

lazy val commonJvmSettings = Seq(
  parallelExecution in Test := false,
  coverageExcludedPackages := "shapeless.examples.*"
)

lazy val coreSettings = buildSettings ++ commonSettings ++ publishSettings ++
  releaseSettings ++ scoverageSettings

lazy val root = project.in(file("."))
  .configure(profile)
  .aggregate(coreJS, coreJVM)
  .dependsOn(coreJS, coreJVM)
  .settings(coreSettings:_*)
  .settings(noPublishSettings)

lazy val CrossTypeMixed: CrossType = new CrossType {
  def projectDir(crossBase: File, projectType: String): File =
    crossBase / projectType

  def sharedSrcDir(projectBase: File, conf: String): Option[File] =
    Some(projectBase.getParentFile / "src" / conf / "scala")
}

lazy val core = crossProject.crossType(CrossTypeMixed)
  .configureCross(configureJUnit)
  .configure(profile)
  .settings(moduleName := "shapeless")
  .settings(coreSettings:_*)
  .configureCross(buildInfoSetup)
  .settings(osgiSettings:_*)
  .settings(
    sourceGenerators in Compile <+= (sourceManaged in Compile).map(Boilerplate.gen)
  )
  .settings(mimaSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val scratch = crossProject.crossType(CrossType.Pure)
  .configureCross(configureJUnit)
  .configure(profile)
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
  .configureCross(configureJUnit)
  .configure(profile)
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
    "org.typelevel" %% "macro-compat" % "1.1.1",
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
      (unmanagedSourceDirectories in sc ).value.map { dir: File =>
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, y)) if y == 10 => new File(dir.getPath + "_2.10")
          case Some((2, y)) if y >= 11 => new File(dir.getPath + "_2.11+")
        }
      }
    }
  }

lazy val publishSettings = Seq(
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
  previousArtifacts := {
    if(scalaVersion.value == "2.12.0-M5") Set()
    else {
      val previousVersion = "2.3.0"
      val previousSJSVersion = "0.6.7"
      val previousSJSBinaryVersion =
        ScalaJSCrossVersion.binaryScalaJSVersion(previousSJSVersion)
      val previousBinaryCrossVersion =
        CrossVersion.binaryMapped(v => s"sjs${previousSJSBinaryVersion}_$v")
      val scalaV = scalaVersion.value
      val scalaBinaryV = scalaBinaryVersion.value
      val thisProjectID = projectID.value
      val previousCrossVersion = thisProjectID.crossVersion match {
        case ScalaJSCrossVersion.binary => previousBinaryCrossVersion
        case crossVersion               => crossVersion
      }

      // Filter out e:info.apiURL as it expects 0.6.7-SNAPSHOT, whereas the
      // artifact we're looking for has 0.6.6 (for example).
      val prevExtraAttributes =
        thisProjectID.extraAttributes.filterKeys(_ != "e:info.apiURL")
      val prevProjectID =
        (thisProjectID.organization % thisProjectID.name % previousVersion)
          .cross(previousCrossVersion)
          .extra(prevExtraAttributes.toSeq: _*)

      Set(CrossVersion(scalaV, scalaBinaryV)(prevProjectID).cross(CrossVersion.Disabled))
    }
  },

  binaryIssueFilters ++= {
    import com.typesafe.tools.mima.core._
    import com.typesafe.tools.mima.core.ProblemFilters._

    // Filtering the methods that were added since the checked version
    // (these only break forward compatibility, not the backward one)
    Seq(
      exclude[MissingMethodProblem]("shapeless.:+:.eliminate"),
      exclude[MissingMethodProblem]("shapeless.CaseClassMacros.shapeless$CaseClassMacros$$$anonfun$15"),
      exclude[MissingMethodProblem]("shapeless.CaseClassMacros.shapeless$CaseClassMacros$$$anonfun$16"),
      exclude[MissingMethodProblem]("shapeless.CaseClassMacros.shapeless$CaseClassMacros$$$anonfun$17"),
      exclude[MissingMethodProblem]("shapeless.UnwrappedInstances.tagUnwrapped"),
      exclude[MissingMethodProblem]("shapeless.CaseClassMacros.findField"),
      exclude[MissingMethodProblem]("shapeless.CaseClassMacros.FieldType")
    )
  }
)

def buildInfoSetup(crossProject: CrossProject): CrossProject = {
  def transform(project: Project) = project enablePlugins BuildInfoPlugin settings (
    buildInfoPackage := "shapeless",
    buildInfoUsePackageAsPath := true,
    buildInfoKeys := Seq[BuildInfoKey](version, scalaVersion, gitHeadCommit),
    buildInfoOptions += BuildInfoOption.BuildTime
  )
  crossProject jvmConfigure transform jsConfigure transform
}

lazy val osgiSettings = defaultOsgiSettings ++ Seq(
  OsgiKeys.bundleSymbolicName := "shapeless",
  OsgiKeys.exportPackage := Seq("shapeless.*;version=${Bundle-Version}"),
  OsgiKeys.importPackage := Seq("""!scala.quasiquotes,scala.*;version="$<range;[==,=+);$<@>>""""),
  OsgiKeys.additionalHeaders := Map("-removeheaders" -> "Include-Resource,Private-Package")
)

lazy val tagName = Def.setting{
  s"shapeless-${if (releaseUseGlobalVersion.value) (version in ThisBuild).value else version.value}"
}

lazy val releaseSettings = Seq(
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  releaseTagName := tagName.value,
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

credentials in ThisBuild ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
