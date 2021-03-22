import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

import com.typesafe.sbt.SbtGit._
import GitKeys._

import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import sbtcrossproject.CrossProject

val Scala211 = "2.11.12"
val Scala212 = "2.12.13"
val Scala213 = "2.13.5"

commonSettings
noPublishSettings
crossScalaVersions := Nil

ThisBuild / organization := "com.chuusai"
ThisBuild / scalaVersion := Scala213
ThisBuild / crossScalaVersions := Seq(Scala211, Scala212, Scala213)
ThisBuild / mimaFailOnNoPrevious := false

// GHA configuration

ThisBuild / githubWorkflowBuildPreamble := Seq(
  WorkflowStep.Run(List("sudo apt install clang libunwind-dev libgc-dev libre2-dev"))
)
ThisBuild / githubWorkflowJavaVersions := Seq("adopt@1.8")
ThisBuild / githubWorkflowBuildMatrixAdditions +=
  "platform" -> List("jvm", "js", "native")

ThisBuild / githubWorkflowArtifactUpload := false
ThisBuild / githubWorkflowBuildMatrixFailFast := Some(false)

val JvmCond = s"matrix.platform == 'jvm'"
val JsCond = s"matrix.platform == 'js'"
val NativeCond = s"matrix.platform == 'native'"

ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Sbt(List("validateJVM"), name = Some("Validate JVM"), cond = Some(JvmCond)),
  WorkflowStep.Sbt(List("validateJS"), name = Some("Validate JavaScript"), cond = Some(JsCond)),
  WorkflowStep.Sbt(List("validateNative"), name = Some("Validate Scala Native"), cond = Some(NativeCond))
)

ThisBuild / githubWorkflowTargetTags ++= Seq("v*")
ThisBuild / githubWorkflowPublishTargetBranches +=
  RefPredicate.StartsWith(Ref.Tag("v"))

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

Global / excludeLintKeys += coreNative / packageDoc / publishArtifact

addCommandAlias("root", ";project shapeless")
addCommandAlias("core", ";project coreJVM")
addCommandAlias("scratch", ";project scratchJVM")
addCommandAlias("examples", ";project examplesJVM")

addCommandAlias("validate", ";root;validateJVM;validateJS;validateNative")
addCommandAlias("validateJVM", ";coreJVM/compile;coreJVM/mimaReportBinaryIssues;coreJVM/test;examplesJVM/compile;examplesJVM/test;coreJVM/doc")
addCommandAlias("validateJS", ";coreJS/compile;coreJS/mimaReportBinaryIssues;coreJS/test;examplesJS/compile;examplesJS/test;coreJS/doc")
addCommandAlias("validateNative", ";coreNative/compile;nativeTest/run;examplesNative/compile")
addCommandAlias("runAll", ";examplesJVM/runAll")

lazy val scoverageSettings = Seq(
  coverageMinimum := 60,
  coverageFailOnMinimum := false,
  coverageExcludedFiles := ".*/src/test/.*"
)

val scalacOptionsAll = Seq(
  "-feature",
  "-language:higherKinds,implicitConversions",
  "-Xfatal-warnings",
  "-deprecation",
  "-unchecked",
)

val scalacOptions212 = Seq(
  "-Xlint:-adapted-args,-delayedinit-select,-nullary-unit,-package-object-classes,-type-parameter-shadow,_",
  "-Ywarn-unused:-implicits"
)

val scalacOptions213 = Seq(
  "-Xlint:-adapted-args,-delayedinit-select,-nullary-unit,-package-object-classes,-type-parameter-shadow,-byname-implicit,_",
  "-Ywarn-unused:-implicits",
  "-Wconf:msg=shadowing a nested class of a parent is deprecated:s"
)

lazy val commonSettings = Seq(
  incOptions := incOptions.value.withLogRecompileOnMacro(false),

  scalacOptions := scalacOptionsAll,

  scalacOptions in compile in Compile ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, y)) if y == 12 => scalacOptions212
    case Some((2, y)) if y >= 13 => scalacOptions213
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
  scalacOptions in (Compile, doc) -= "-Xfatal-warnings",
  parallelExecution in Test := false,
  coverageEnabled := false
)

lazy val commonJvmSettings = Seq(
  parallelExecution in Test := false,
  coverageExcludedPackages := "shapeless.examples.*"
)

lazy val coreSettings = commonSettings ++ publishSettings

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
  .settings(sourceGenerators in Compile += (sourceManaged in Compile).map(Boilerplate.gen).taskValue)
  .settings(mimaSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)
  .jvmSettings(scoverageSettings:_*)
  .nativeSettings(
    // disable scaladoc generation on native
    // currently getting errors like
    //   [error] bnd: Invalid syntax for version: ${@}, for cmd: range, arguments; [range, [==,=+), ${@}]
    publishArtifact in (Compile, packageDoc) := false,
    publishArtifact in packageDoc := false,
    sources in (Compile,doc) := Nil,
    sources in Test := Nil
  )

lazy val coreJVM = core.jvm
lazy val coreJS = core.js
lazy val coreNative = core.native

lazy val scratch = crossProject(JSPlatform, JVMPlatform, NativePlatform).crossType(CrossTypeMixed)
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

lazy val examples = crossProject(JSPlatform, JVMPlatform, NativePlatform).crossType(CrossTypeMixed)
  .configureCross(configureJUnit)
  .dependsOn(core)
  .settings(moduleName := "examples")
  .settings(
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, scalaMajor)) if scalaMajor >= 11 =>
          Seq("org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2")
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
    sources in Compile ~= (_.filterNot(_.getName == "sexp.scala")),
    sources in Test := Nil
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
    scalaOrganization.value % "scala-reflect" % scalaVersion.value % "provided",
    scalaOrganization.value % "scala-compiler" % scalaVersion.value % "provided"
  )
)

lazy val crossVersionSharedSources: Seq[Setting[_]] =
  Seq(Compile, Test).map { sc =>
    (unmanagedSourceDirectories in sc) ++= {
      (unmanagedSourceDirectories in sc ).value.flatMap { dir: File =>
        if(dir.getName != "scala") Seq(dir)
        else
          CrossVersion.partialVersion(scalaVersion.value) match {
            case Some((2, y)) if y >= 13 => Seq(new File(dir.getPath + "_2.13+"))
            case Some((2, y)) if y >= 11 => Seq(new File(dir.getPath + "_2.13-"))
          }
      }
    }
  }

lazy val publishSettings = Seq(
  publishArtifact in Test := false,
  pomIncludeRepository := (_ => false),
  homepage := Some(url("https://github.com/milessabin/shapeless")),
  licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  scmInfo := Some(ScmInfo(url("https://github.com/milessabin/shapeless"), "scm:git:git@github.com:milessabin/shapeless.git")),
  developers := List(
    Developer("milessabin", "Miles Sabin", "", url("http://milessabin.com/blog")),
    Developer("joroKr21", "Georgi Krastev", "joro.kr.21@gmail.com", url("https://twitter.com/Joro_Kr"))
  )
)

lazy val noPublishSettings =
  skip in publish := true

enablePlugins(MimaPlugin)
lazy val mimaSettings = Seq(
  mimaPreviousArtifacts := {
    val previousVersion = if(scalaVersion.value.startsWith("2.13.")) "2.3.3" else "2.3.2"
    Set(organization.value %% moduleName.value % previousVersion)
  },
  mimaBinaryIssueFilters := Seq(
    // Macro internals - ignore
    exclude[InheritedNewAbstractMethodProblem]("shapeless.CaseClassMacros.varargTC"),
    exclude[InheritedNewAbstractMethodProblem]("shapeless.CaseClassMacros.varargTpt"),
    exclude[InheritedNewAbstractMethodProblem]("shapeless.CaseClassMacros.shapeless$CaseClassMacrosVersionSpecifics$_setter_$varargTC_="),
    exclude[InheritedNewAbstractMethodProblem]("shapeless.CaseClassMacros.shapeless$CaseClassMacrosVersionSpecifics$_setter_$varargTpt_="),
    exclude[InheritedNewAbstractMethodProblem]("shapeless.IsCons1Macros.varargTC"),
    exclude[InheritedNewAbstractMethodProblem]("shapeless.IsCons1Macros.varargTpt"),
    exclude[InheritedNewAbstractMethodProblem]("shapeless.IsCons1Macros.shapeless$CaseClassMacrosVersionSpecifics$_setter_$varargTC_="),
    exclude[InheritedNewAbstractMethodProblem]("shapeless.IsCons1Macros.shapeless$CaseClassMacrosVersionSpecifics$_setter_$varargTpt_="),
    exclude[MissingClassProblem]("shapeless.CaseClassMacros$PatchedContext$2$PatchedLookupResult"),
    exclude[MissingTypesProblem]("shapeless.LazyMacros$"),
    exclude[MissingClassProblem]("shapeless.LazyMacrosCompat"),
    exclude[MissingClassProblem]("shapeless.LazyMacrosRef"),

    // 2.13.x collections related - to eliminate somehow
    exclude[DirectMissingMethodProblem]("shapeless.AdditiveCollection.indexedSeqAdditiveCollection"),
    exclude[IncompatibleMethTypeProblem]("shapeless.SizedOps.++"),
    exclude[DirectMissingMethodProblem]("shapeless.Sized.apply"),
    exclude[DirectMissingMethodProblem]("shapeless.SizedBuilder.apply"),
    exclude[IncompatibleMethTypeProblem]("shapeless.SizedBuilder.apply"),
    exclude[IncompatibleMethTypeProblem]("shapeless.syntax.SizedConv.this"),
    exclude[IncompatibleMethTypeProblem]("shapeless.syntax.sized.genTraversableSizedConv"),
    exclude[IncompatibleMethTypeProblem]("shapeless.syntax.std.traversable.traversableOps2"),
    exclude[IncompatibleMethTypeProblem]("shapeless.syntax.std.TraversableOps2.this"),
    exclude[IncompatibleMethTypeProblem]("shapeless.ops.hlist#Repeat.apply"),
    exclude[IncompatibleMethTypeProblem]("shapeless.ops.traversable#ToSizedHList.instance"),
    exclude[DirectMissingMethodProblem]("shapeless.ops.hlist#ToSized.hlistToSized"),
    exclude[IncompatibleMethTypeProblem]("shapeless.ops.traversable#FromTraversable.apply"),
    exclude[ReversedMissingMethodProblem]("shapeless.ops.traversable#FromTraversable.apply"),

    // Other
    exclude[IncompatibleResultTypeProblem]("shapeless.syntax.HListOps.toCoproduct"),
    exclude[InheritedNewAbstractMethodProblem]("shapeless.ops.hlist#Repeat.apply"),
    exclude[ReversedMissingMethodProblem]("shapeless.SingletonTypeUtils.isSymbolLiteral"),
    exclude[ReversedMissingMethodProblem]("shapeless.CaseClassMacros.numNonCaseParamLists"),

    // All the following were present in 2.3.2 ...

    // Filtering the methods that were added since the checked version
    // (these only break forward compatibility, not the backward one)
    exclude[ReversedMissingMethodProblem]("shapeless.ops.hlist#IsHCons.cons"),

    // Filtering removals
    exclude[ReversedMissingMethodProblem]("shapeless.ops.coproduct#IsCCons.cons"),
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
