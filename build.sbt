import com.typesafe.sbt.SbtGit.GitKeys.*
import com.typesafe.tools.mima.core.*
import sbtcrossproject.CrossPlugin.autoImport.crossProject
import sbtcrossproject.CrossProject

val Scala212 = "2.12.20"
val Scala213 = "2.13.16"

commonSettings
noPublishSettings
crossScalaVersions := Nil

ThisBuild / organization := "com.chuusai"
ThisBuild / scalaVersion := Scala213
ThisBuild / crossScalaVersions := Seq(Scala212, Scala213)
ThisBuild / mimaFailOnNoPrevious := false
ThisBuild / versionScheme := Some("pvp")

// GHA configuration

ThisBuild / githubWorkflowBuildPreamble := Seq(WorkflowStep.Run(List("sudo apt install clang libunwind-dev libgc-dev libre2-dev")))
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("8"))
ThisBuild / githubWorkflowArtifactUpload := false
ThisBuild / githubWorkflowBuildMatrixAdditions += "platform" -> List("jvm", "js", "native")
ThisBuild / githubWorkflowBuildMatrixFailFast := Some(false)
ThisBuild / githubWorkflowTargetBranches := Seq("**") // match all branches, including slashes

val JvmCond = s"matrix.platform == 'jvm'"
val JsCond = s"matrix.platform == 'js'"
val NativeCond = s"matrix.platform == 'native'"

ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Sbt(List("validateJVM"), name = Some("Validate JVM"), cond = Some(JvmCond)),
  WorkflowStep.Sbt(List("validateJS"), name = Some("Validate JavaScript"), cond = Some(JsCond)),
  WorkflowStep.Sbt(List("validateNative"), name = Some("Validate Scala Native"), cond = Some(NativeCond))
)

ThisBuild / githubWorkflowTargetTags ++= Seq("v*")
ThisBuild / githubWorkflowPublishTargetBranches ++= Seq(RefPredicate.Equals(Ref.Branch("series/2.3")), RefPredicate.StartsWith(Ref.Tag("v")))
ThisBuild / githubWorkflowPublishPreamble += WorkflowStep.Use(UseRef.Public("olafurpg", "setup-gpg", "v3"))
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

addCommandAlias("root", ";project shapeless")
addCommandAlias("core", ";project coreJVM")
addCommandAlias("scratch", ";project scratchJVM")
addCommandAlias("examples", ";project examplesJVM")

addCommandAlias("validate", ";root;validateJVM;validateJS;validateNative")
addCommandAlias("validateJVM", ";coreJVM/compile;coreJVM/mimaReportBinaryIssues;coreJVM/test;examplesJVM/compile;examplesJVM/test;examplesJVM/runAll;coreJVM/doc")
addCommandAlias("validateJS", ";coreJS/compile;coreJS/mimaReportBinaryIssues;coreJS/test;examplesJS/compile;examplesJS/test;examplesJS/runAll;coreJS/doc")
addCommandAlias("validateNative", ";coreNative/compile;coreNative/test;examplesNative/compile;examplesNative/test;examplesNative/runAll;coreNative/doc")
addCommandAlias("runAll", ";examplesJVM/runAll")
addCommandAlias("mima", ";coreJVM/mimaReportBinaryIssues")

val scalacOptionsAll = Seq(
  "-feature",
  "-language:higherKinds,implicitConversions",
  "-Xfatal-warnings",
  "-deprecation",
  "-unchecked",
)

val scalacCompileOptions = Map(
  "2.12" -> Seq(
    "-Xlint:-adapted-args,-delayedinit-select,-nullary-unit,-package-object-classes,-type-parameter-shadow,_",
    "-Ywarn-unused:-implicits",
  ),
  "2.13" -> Seq(
    "-Xlint:-adapted-args,-delayedinit-select,-nullary-unit,-package-object-classes,-type-parameter-shadow,-byname-implicit,_",
    "-Wunused:-implicits",
    "-Wconf:msg=shadowing a nested class of a parent is deprecated:s",
  ),
)

val scalacTestOptions = Map(
  "2.12" -> Seq(
    "-Xlint:-infer-any",
    "-Ywarn-unused:-locals,-privates",
  ),
  "2.13" -> Seq(
    "-Xlint:-infer-any",
    "-Wunused:-locals,-privates",
    // Symbol.unapply returns Option
    "-Wconf:cat=other-implicit-type:s,cat=other-match-analysis&src=*/lazy.scala:s",
  ),
)

lazy val commonSettings = crossVersionSharedSources ++ Seq(
  resolvers ++= Resolver.sonatypeOssRepos("releases"),
  resolvers ++= Resolver.sonatypeOssRepos("snapshots"),
  incOptions := incOptions.value.withLogRecompileOnMacro(false),
  scalacOptions := scalacOptionsAll,
  Compile / scalacOptions ++= scalacCompileOptions.getOrElse(scalaBinaryVersion.value, Nil),
  Test / scalacOptions ++= scalacTestOptions.getOrElse(scalaBinaryVersion.value, Nil),
  Compile / console / scalacOptions -= "-Xfatal-warnings",
  Test / console / scalacOptions -= "-Xfatal-warnings",
  console / initialCommands := """import shapeless._""",
  Test / parallelExecution := false,
  libraryDependencies ++= Seq(
    scalaOrganization.value % "scala-reflect" % scalaVersion.value % "provided",
    scalaOrganization.value % "scala-compiler" % scalaVersion.value % "provided"
  )
)

def configureJUnit(crossProject: CrossProject) = crossProject
  .jvmSettings(libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % "test")
  .jsConfigure(_.enablePlugins(ScalaJSJUnitPlugin))
  .nativeConfigure(_.enablePlugins(ScalaNativeJUnitPlugin))

val boilerplate = Def.taskDyn {
  (Compile / sourceManaged).map(Boilerplate.gen(scalaBinaryVersion.value))
}

lazy val macroAnnotationSettings = Seq(
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, v)) if v >= 13 => Seq("-Ymacro-annotations")
      case _ => Nil
    }
  },
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, v)) if v <= 12 =>
        Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full))
      case _ => Nil
    }
  },
)

lazy val coreTestMacros = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .settings(moduleName := "core-test-macros")
  .settings(commonSettings)
  .settings(noPublishSettings)
  .configureCross(buildInfoSetup)
  .settings(macroAnnotationSettings)

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .configureCross(configureJUnit)
  .settings(moduleName := "shapeless")
  .settings(commonSettings)
  .settings(publishSettings)
  .configureCross(buildInfoSetup)
  .enablePlugins(SbtOsgi)
  .settings(coreOsgiSettings)
  .settings(Compile / sourceManaged := baseDirectory.value.getParentFile / "shared" / "src" / "main" / "managed")
  .settings(Compile / sourceGenerators += boilerplate.taskValue)
  .settings(mimaSettings)
  .dependsOn(coreTestMacros % "test->compile")
  .settings(macroAnnotationSettings)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js
lazy val coreNative = core.native

lazy val scratch = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .configureCross(configureJUnit)
  .dependsOn(core)
  .settings(moduleName := "scratch")
  .settings(commonSettings)
  .settings(noPublishSettings)

lazy val scratchJVM = scratch.jvm
lazy val scratchJS = scratch.js
lazy val scratchNative = scratch.native

lazy val runAll = TaskKey[Unit]("runAll")
def runAllIn(config: Configuration): Setting[Task[Unit]] = {
  config / runAll := {
    val classes = (config / discoveredMainClasses).value
    val runner0 = (run / runner).value
    val cp = (config / fullClasspath).value
    val s = streams.value
    classes.foreach(c => runner0.run(c, Attributed.data(cp), Nil, s.log))
  }
}

lazy val examples = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .configureCross(configureJUnit)
  .dependsOn(core)
  .settings(moduleName := "examples")
  .settings(libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.4.0")
  .settings(runAllIn(Compile))
  .settings(commonSettings)
  .settings(noPublishSettings)
  .settings(scalacOptions ++= scalacTestOptions.getOrElse(scalaBinaryVersion.value, Nil))
  .nativeSettings(Compile / sources ~= (_.filterNot(_.getName == "sexp.scala")))

lazy val examplesJVM = examples.jvm
lazy val examplesJS = examples.js
lazy val examplesNative = examples.native

lazy val crossVersionSharedSources: Seq[Setting[?]] =
  Seq(Compile, Test).map { sc =>
    (sc / unmanagedSourceDirectories) ++= {
      (sc / unmanagedSourceDirectories).value.flatMap { dir: File =>
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
  Test / publishArtifact := false,
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
  publish / skip := true

enablePlugins(MimaPlugin)
lazy val mimaSettings = Seq(
  mimaPreviousArtifacts := Set(organization.value %% moduleName.value % "2.3.3"),
  mimaBinaryIssueFilters := List(
    // removed internal macro classes
    ProblemFilters.exclude[MissingTypesProblem]("shapeless.LazyMacros$"),
    ProblemFilters.exclude[MissingClassProblem]("shapeless.LazyMacrosRef"),
    ProblemFilters.exclude[MissingClassProblem]("shapeless.LazyMacrosCompat"),
    // removed private classes and methods
    ProblemFilters.exclude[MissingClassProblem]("shapeless.ScalaVersionSpecifics$macrocompat$"),
    ProblemFilters.exclude[MissingClassProblem]("shapeless.ScalaVersionSpecifics$macrocompat$bundle"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("shapeless.ScalaVersionSpecifics.macrocompat"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("shapeless.package.macrocompat"),
    // inaccessible interface change
    ProblemFilters.exclude[MissingClassProblem]("shapeless.CaseClassMacros$PatchedContext$2$PatchedLookupResult"),
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
