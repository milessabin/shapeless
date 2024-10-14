import com.github.sbt.git.SbtGit.GitKeys.*
import sbtcrossproject.CrossProject

val Scala212 = "2.12.20"
val Scala213 = "2.13.11"

commonSettings
noPublishSettings
crossScalaVersions := Nil

ThisBuild / organization := "com.chuusai"
ThisBuild / scalaVersion := Scala213
ThisBuild / crossScalaVersions := Seq(Scala212, Scala213)
ThisBuild / mimaFailOnNoPrevious := false

// GHA configuration

ThisBuild / githubWorkflowBuildPreamble := Seq(WorkflowStep.Run(List("sudo apt install clang libunwind-dev libgc-dev libre2-dev")))
ThisBuild / githubWorkflowJavaVersions := Seq("adopt@1.8")
ThisBuild / githubWorkflowBuildMatrixAdditions += "platform" -> List("jvm", "js", "native")
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
ThisBuild / githubWorkflowPublishTargetBranches += RefPredicate.StartsWith(Ref.Tag("v"))
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

def scalacOptionsAll(pluginJar: File) = List(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:higherKinds,implicitConversions",
  "-Xfatal-warnings",
  "-Wconf:cat=other-implicit-type:s",
  s"-Xplugin:${pluginJar.getAbsolutePath}",
  s"-Jdummy=${pluginJar.lastModified}"
)

lazy val commonSettings = crossVersionSharedSources ++ Seq(
  resolvers ++= Resolver.sonatypeOssRepos("releases"),
  resolvers ++= Resolver.sonatypeOssRepos("snapshots"),
  incOptions := incOptions.value.withLogRecompileOnMacro(false),
  scalacOptions := scalacOptionsAll((plugin / Compile / packageBin).value),
  Compile / compile / scalacOptions ++= Seq(
    "-Ywarn-unused:-implicits",
    "-Xlint:-adapted-args,-delayedinit-select,-nullary-unit,-package-object-classes,-type-parameter-shadow,_"
  ),
  Compile / compile / scalacOptions ++= (scalaBinaryVersion.value match {
    case "2.13" => Seq("-Xlint:-byname-implicit")
    case _ => Nil
  }),
  Compile / console / scalacOptions -= "-Xfatal-warnings",
  Test / console / scalacOptions -= "-Xfatal-warnings",
  console / initialCommands := """import shapeless._""",
  Test / parallelExecution := false,
  libraryDependencies ++= Seq(
    scalaOrganization.value % "scala-reflect" % scalaVersion.value % "provided",
    scalaOrganization.value % "scala-compiler" % scalaVersion.value % "provided"
  )
)

lazy val javaModuleName = settingKey[String]("Java module name")

def configureJUnit(crossProject: CrossProject) = crossProject
  .jvmSettings(libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % "test")
  .jsConfigure(_.enablePlugins(ScalaJSJUnitPlugin))
  .nativeConfigure(_.enablePlugins(ScalaNativeJUnitPlugin))

lazy val plugin = project.in(file("plugin"))
  .settings(crossVersionSharedSources)
  .settings(publishSettings)
  .settings(
    name := "shapeless-plugin",
    moduleName := "shapeless-plugin",
    javaModuleName := "shapeless.plugin",
    sbtPlugin := true,
    scalaVersion := Scala213,
    crossScalaVersions := Seq(Scala213, Scala212)
  )

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
  .settings(
    moduleName := "shapeless",
    javaModuleName := "shapeless.core"
  )
  .settings(commonSettings)
  .settings(publishSettings)
  .configureCross(buildInfoSetup)
  .enablePlugins(SbtOsgi)
  .settings(coreOsgiSettings)
  .settings(Compile / sourceManaged := baseDirectory.value.getParentFile / "shared" / "src" / "main" / "managed")
  .settings(Compile / sourceGenerators += (Compile / sourceManaged).map(Boilerplate.gen).taskValue)
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
  .settings(
    moduleName := "scratch",
    javaModuleName := "shapeless.scratch"
  )
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
  .nativeSettings(Compile / sources ~= (_.filterNot(_.getName == "sexp.scala")))

lazy val examplesJVM = examples.jvm
lazy val examplesJS = examples.js
lazy val examplesNative = examples.native

lazy val crossVersionSharedSources: Seq[Setting[?]] =
  Seq(Compile, Test).map { sc =>
    (sc / unmanagedSourceDirectories) ++= {
      (sc / unmanagedSourceDirectories).value.flatMap { dir: File =>
        if (dir.getName != "scala") Seq(dir)
        else CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, y)) if y >= 13 => Seq(new File(dir.getPath + "_2.13+"))
          case Some((2, y)) if y <  13 => Seq(new File(dir.getPath + "_2.13-"))
        }
      }
    }
  }

lazy val publishSettings = Seq(
  Test / publishArtifact := false,
  pomIncludeRepository := (_ => false),
  homepage := Some(url("https://github.com/milessabin/shapeless")),
  licenses := Seq("Apache 2" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt")),
  scmInfo := Some(ScmInfo(url("https://github.com/milessabin/shapeless"), "scm:git:git@github.com:milessabin/shapeless.git")),
  developers := List(
    Developer("milessabin", "Miles Sabin", "", url("https://milessabin.com/blog")),
    Developer("joroKr21", "Georgi Krastev", "joro.kr.21@gmail.com", url("https://twitter.com/Joro_Kr"))
  ),
  packageOptions += Package.ManifestAttributes("Automatic-Module-Name" -> javaModuleName.value),
)

lazy val noPublishSettings =
  publish / skip := true

enablePlugins(MimaPlugin)
lazy val mimaSettings = Seq(
  mimaPreviousArtifacts := Set(),
  mimaBinaryIssueFilters := Seq()
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
