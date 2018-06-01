import com.typesafe.sbt.pgp.PgpKeys.publishSigned
import org.scalajs.sbtplugin.ScalaJSCrossVersion
import ReleaseTransformations._

import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings

import com.typesafe.sbt.SbtGit._
import GitKeys._

import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import sbtcrossproject.CrossProject

inThisBuild(Seq(
  organization := "com.chuusai",
  scalaVersion := "2.13.0-M4",
  crossScalaVersions := Seq("2.10.7", "2.11.12", "2.12.6", "2.13.0-M4")
))

addCommandAlias("root", ";project root")
addCommandAlias("core", ";project coreJVM")
addCommandAlias("scratch", ";project scratchJVM")
addCommandAlias("examples", ";project examplesJVM")

addCommandAlias("validate", ";root;validateJVM;validateJS")
addCommandAlias("validateJVM", ";coreJVM/compile;coreJVM/mimaReportBinaryIssues;coreJVM/test;coreJVM/doc")
addCommandAlias("validateJS", ";coreJS/compile;coreJS/mimaReportBinaryIssues;coreJS/test;coreJS/doc")
addCommandAlias("validateNative", ";coreNative/compile;nativeTest/run")

addCommandAlias("runAll", ";examplesJVM/runAll")
addCommandAlias("releaseAll", ";root;release skip-tests")

lazy val scoverageSettings = Seq(
  coverageMinimum := 60,
  coverageFailOnMinimum := false,
  coverageExcludedFiles := ".*/src/test/.*"
)

lazy val commonSettings = Seq(
  incOptions := incOptions.value.withLogRecompileOnMacro(false),

  scalacOptions := Seq(
    "-feature",
    "-Xfuture",
    "-language:higherKinds,implicitConversions",
    //"-Xfatal-warnings",
    "-deprecation",
    "-unchecked",
    "-Ymacro-annotations"
  ),
  scalacOptions in compile in Compile ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 12)) =>
      "-Xlint:-adapted-args,-delayedinit-select,-nullary-unit,-package-object-classes,-type-parameter-shadow,_" ::
      "-Ywarn-unused:-implicits" :: Nil
    case _ => Nil
  }),

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
      "com.novocode" % "junit-interface" % "0.11" % "test"
  )
}

lazy val commonJsSettings = Seq(
  scalacOptions += {
    val tagOrHash = {
      val tag = tagName.value
      if(isSnapshot.value) sys.process.Process("git rev-parse HEAD").lineStream_!.head
      else tag
    }
    val a = (baseDirectory in LocalRootProject).value.toURI.toString
    val g = "https://raw.githubusercontent.com/milessabin/shapeless/" + tagOrHash
    s"-P:scalajs:mapSourceURI:$a->$g/"
  },
  parallelExecution in Test := false,
  coverageExcludedPackages := ".*"
)

lazy val commonJvmSettings = Seq(
  parallelExecution in Test := false,
  coverageExcludedPackages := "shapeless.examples.*"
)

lazy val coreSettings = commonSettings ++ publishSettings ++
  releaseSettings ++ scoverageSettings

lazy val root = project.in(file("."))
  .aggregate(coreJS, coreJVM)
  .dependsOn(coreJS, coreJVM)
  .settings(coreSettings:_*)
  .settings(noPublishSettings)

lazy val CrossTypeMixed: sbtcrossproject.CrossType = new sbtcrossproject.CrossType {
  def projectDir(crossBase: File, projectType: String): File =
    crossBase / projectType

  override def projectDir(crossBase: File, projectType: sbtcrossproject.Platform) = {
    val dir = projectType match {
      case JVMPlatform => "jvm"
      case JSPlatform => "js"
      case NativePlatform => "native"
    }
    crossBase / dir
  }

  def sharedSrcDir(projectBase: File, conf: String): Option[File] =
    Some(projectBase.getParentFile / "src" / conf / "scala")
}

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform).crossType(CrossTypeMixed)
  .configureCross(configureJUnit)
  .settings(moduleName := "shapeless")
  .settings(coreSettings:_*)
  .configureCross(buildInfoSetup)
  .enablePlugins(SbtOsgi)
  .settings(coreOsgiSettings:_*)
  .settings(
    sourceGenerators in Compile += (sourceManaged in Compile).map(Boilerplate.gen).taskValue
  )
  .settings(mimaSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)
  .nativeSettings(
    // disable scaladoc generation on native
    // currently getting errors like
    //   [error] bnd: Invalid syntax for version: ${@}, for cmd: range, arguments; [range, [==,=+), ${@}]
    publishArtifact in (Compile, packageDoc) := false,
    publishArtifact in packageDoc := false,
    sources in (Compile,doc) := Seq.empty
  )

lazy val coreJVM = core.jvm
lazy val coreJS = core.js
lazy val coreNative = core.native

lazy val scratch = crossProject(JSPlatform, JVMPlatform, NativePlatform).crossType(CrossType.Pure)
  .configureCross(configureJUnit)
  .dependsOn(core)
  .settings(moduleName := "scratch")
  .settings(coreSettings:_*)
  .settings(noPublishSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

lazy val scratchJVM = scratch.jvm
lazy val scratchJS = scratch.js
lazy val scratchNative = scratch.native

lazy val runAll = TaskKey[Unit]("runAll")

def runAllIn(config: Configuration): Setting[Task[Unit]] = {
  runAll in config := {
    val classes = (discoveredMainClasses in config).value
    val runner0 = (runner in run).value
    val cp = (fullClasspath in config).value
    val s = streams.value
    classes.foreach(c => runner0.run(c, Attributed.data(cp), Seq(), s.log))
  }
}

lazy val examples = crossProject(JSPlatform, JVMPlatform, NativePlatform).crossType(CrossType.Pure)
  .configureCross(configureJUnit)
  .dependsOn(core)
  .settings(moduleName := "examples")
  .settings(
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, scalaMajor)) if scalaMajor >= 11 =>
          Seq("org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0")
        case _ => Seq()
      }
    }
  )
  .settings(runAllIn(Compile))
  .settings(coreSettings:_*)
  .settings(noPublishSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)
  .nativeSettings(
    sources in Compile ~= {
      _.filterNot(_.getName == "sexp.scala")
    }
  )

lazy val examplesJVM = examples.jvm
lazy val examplesJS = examples.js
lazy val examplesNative = examples.native

lazy val nativeTest = project
  .enablePlugins(ScalaNativePlugin)
  .settings(
    noPublishSettings,
    sourceGenerators in Compile += Def.task {
      val exclude = List(
        "StagedTypeClassExample", // scala-reflect
        "CombinatorTesting", // scala-parser-combinators
        "ALaCacheDemo" // java.util.WeakHashMap, java.util.logging.Logger
      )
      val classNames = (discoveredMainClasses in Compile in examplesNative).value.filterNot{
        c => exclude.exists(c.contains)
      }.sorted
      val src = s"""package shapeless
      |
      |object NativeMain {
      |  def main(args: Array[String]): Unit = {
      |${classNames.map("    " + _ + ".main(args)").mkString("\n")}
      |  }
      |}
      |""".stripMargin
      val f = (sourceManaged in Compile).value / "shapeless" / "NativeMain.scala"
      IO.write(f, src)
      f :: Nil
    }.taskValue
  ).dependsOn(
    examplesNative
  )

lazy val scalaMacroDependencies: Seq[Setting[_]] = Seq(
  libraryDependencies ++= Seq(
    "org.typelevel" %% "macro-compat" % "1.1.1",
    scalaOrganization.value % "scala-reflect" % scalaVersion.value % "provided",
    scalaOrganization.value % "scala-compiler" % scalaVersion.value % "provided"
  ),
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      // if scala 2.13+ is used, quasiquotes and macro-annotations are merged into scala-reflect
      case Some((2, scalaMajor)) if scalaMajor >= 13 => Seq()
      // if scala 2.11+ is used, quasiquotes are merged into scala-reflect
      case Some((2, scalaMajor)) if scalaMajor >= 11 => Seq(
          compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch)
        )
      // in Scala 2.10, quasiquotes are provided by macro paradise
      case Some((2, 10)) => Seq(
          compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch),
          "org.scalamacros" %% "quasiquotes" % "2.1.1" cross CrossVersion.binary
        )
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
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (version.value.trim.endsWith("SNAPSHOT"))
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

lazy val noPublishSettings =
  skip in publish := true

lazy val mimaSettings = mimaDefaultSettings ++ Seq(
  mimaPreviousArtifacts := {
    if(scalaVersion.value == "2.13.0-M4") Set()
    else {
      val previousVersion = if(scalaVersion.value == "2.12.6") "2.3.2" else "2.3.0"
      val previousSJSVersion = "0.6.7"
      val previousSJSBinaryVersion =
        ScalaJSCrossVersion.binaryScalaJSVersion(previousSJSVersion)
      val previousBinaryCrossVersion =
        CrossVersion.binaryWith(s"sjs${previousSJSBinaryVersion}_", "")
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

      Set(CrossVersion(scalaV, scalaBinaryV)(prevProjectID).cross(CrossVersion.Disabled()))
    }
  },

  mimaBinaryIssueFilters ++= {
    import com.typesafe.tools.mima.core._
    import com.typesafe.tools.mima.core.ProblemFilters._

    Seq(
      // Filtering the methods that were added since the checked version
      // (these only break forward compatibility, not the backward one)
      exclude[MissingMethodProblem]("shapeless.:+:.eliminate"),
      exclude[MissingMethodProblem]("shapeless.CaseClassMacros.shapeless$CaseClassMacros$$$anonfun$15"),
      exclude[MissingMethodProblem]("shapeless.CaseClassMacros.shapeless$CaseClassMacros$$$anonfun$16"),
      exclude[MissingMethodProblem]("shapeless.CaseClassMacros.shapeless$CaseClassMacros$$$anonfun$17"),
      exclude[MissingMethodProblem]("shapeless.UnwrappedInstances.tagUnwrapped"),
      exclude[MissingMethodProblem]("shapeless.CaseClassMacros.findField"),
      exclude[MissingMethodProblem]("shapeless.CaseClassMacros.FieldType"),
      exclude[MissingMethodProblem]("shapeless.SingletonTypeUtils.parseSingletonSymbolType"),
      exclude[MissingMethodProblem]("shapeless.ops.hlist#IsHCons.cons"),

      // Filtering removals
      exclude[MissingMethodProblem]("shapeless.ops.coproduct#IsCCons.cons"),
      exclude[MissingClassProblem]("shapeless.ops.coproduct$ZipOne$"),
      exclude[MissingClassProblem]("shapeless.ops.coproduct$ZipOne"),
      exclude[DirectMissingMethodProblem]("shapeless.LazyMacros.dcRef"),
      exclude[DirectMissingMethodProblem]("shapeless.LazyMacros.dcRef_="),

      // Implicit reorderings
      exclude[DirectMissingMethodProblem]("shapeless.LowPriorityUnaryTCConstraint.hnilConstUnaryTC"),
      exclude[ReversedMissingMethodProblem]("shapeless.LowPriorityUnaryTCConstraint.hnilUnaryTC"),

      // Relaxed constraints
      exclude[IncompatibleMethTypeProblem]("shapeless.ops.traversable#ToSizedHList.apply"),
      exclude[ReversedMissingMethodProblem]("shapeless.ops.traversable#ToSizedHList.apply")
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
  crossProject jvmConfigure transform jsConfigure transform nativeConfigure transform
}

lazy val coreOsgiSettings = osgiSettings ++ Seq(
  OsgiKeys.bundleSymbolicName := "shapeless",
  OsgiKeys.exportPackage := Seq("shapeless.*;version=${Bundle-Version}"),
  OsgiKeys.importPackage := {
    val Some((major, minor)) = CrossVersion.partialVersion(scalaVersion.value)
    Seq(s"""!scala.quasiquotes,scala.*;version="[$major.$minor,$major.${minor+1})"""")
  },
  OsgiKeys.additionalHeaders := Map("-removeheaders" -> "Include-Resource,Private-Package")
)

lazy val tagName = Def.setting{
  s"shapeless-${if (releaseUseGlobalVersion.value) (version in ThisBuild).value else version.value}"
}

val Scala211 = "2.11.12"

lazy val releaseSettings = Seq(
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  releaseTagName := tagName.value,
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    releaseStepCommand(s"++${Scala211}!"),
    releaseStepCommand("nativeTest/run"),
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    publishArtifacts,
    releaseStepCommand(s"++${Scala211}"),
    releaseStepCommand("coreNative/publishSigned"),
    setNextVersion,
    commitNextVersion,
    releaseStepCommand("sonatypeReleaseAll"),
    pushChanges
  )
)

credentials in ThisBuild ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
