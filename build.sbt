import com.typesafe.sbt.SbtGit.GitKeys._
import sbtcrossproject.CrossProject

val Scala212 = "2.12.14"
val Scala213 = "2.13.6"
val scala3 = "3.0.0"

commonSettings
noPublishSettings
crossScalaVersions := Nil

ThisBuild / organization := "com.chuusai"
ThisBuild / scalaVersion := Scala213
ThisBuild / crossScalaVersions := Seq(Scala212, Scala213, scala3)
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

//TODO Global / excludeLintKeys += coreNative / packageDoc / publishArtifact

addCommandAlias("root", ";project shapeless")
addCommandAlias("core", ";project coreJVM")
addCommandAlias("scratch", ";project scratchJVM")
addCommandAlias("examples", ";project examplesJVM")

addCommandAlias("validate", ";root;validateJVM;validateJS;validateNative")
addCommandAlias("validateJVM", ";coreJVM/compile;coreJVM/mimaReportBinaryIssues;coreJVM/test;examplesJVM/compile;examplesJVM/test;coreJVM/doc")
addCommandAlias("validateJS", ";coreJS/compile;coreJS/mimaReportBinaryIssues;coreJS/test;examplesJS/compile;examplesJS/test;coreJS/doc")
addCommandAlias("validateNative", ";coreNative/compile;nativeTest/run;examplesNative/compile")
addCommandAlias("runAll", ";examplesJVM/runAll")

val scalacOptionsAll = List(
  "-feature",
  "-language:higherKinds,implicitConversions",
  "-Xfatal-warnings",
  "-deprecation",
  "-unchecked"
)

def scalacOptions2(pluginJar: File) = List(
  s"-Xplugin:${pluginJar.getAbsolutePath}",
  s"-Jdummy=${pluginJar.lastModified}"
)

val scalacOptions3 = Seq(
  "-language:dynamics",
)

def scalacOptions212(pluginJar: File) = Seq(
  "-Xlint:-adapted-args,-delayedinit-select,-nullary-unit,-package-object-classes,-type-parameter-shadow,_",
  "-Ywarn-unused:-implicits"
) ++ scalacOptions2(pluginJar)

def scalacOptions213(pluginJar: File) = Seq(
  "-Xlint:-adapted-args,-delayedinit-select,-nullary-unit,-package-object-classes,-type-parameter-shadow,-byname-implicit,_",
  "-Ywarn-unused:-implicits"
) ++ scalacOptions2(pluginJar)

lazy val commonSettings = Seq(
  incOptions := incOptions.value.withLogRecompileOnMacro(false),

  scalacOptions := scalacOptionsAll,

  Compile / compile / scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 12)) => scalacOptions212((plugin / Compile / packageBin).value)
    case Some((2, 13)) => scalacOptions213((plugin / Compile / packageBin).value)
    case Some((3, _)) => scalacOptions3
    case _ => Nil
  }),
  libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, _)) => scalaMacroDependencies.value
    case _ => Nil
  }),

  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),

  Compile / console / scalacOptions -= "-Xfatal-warnings",
  Test / console / scalacOptions -= "-Xfatal-warnings",
  console / initialCommands := """import shapeless._""",

  scmInfo :=
    Some(ScmInfo(
      url("https://github.com/milessabin/shapeless"),
      "scm:git:git@github.com:milessabin/shapeless.git"
    ))
) ++ crossVersionSharedSources

def configureJUnit(crossProject: CrossProject) = {
  crossProject
  .jsConfigure(_.enablePlugins(ScalaJSJUnitPlugin))
  .jvmSettings(
    libraryDependencies +=
      "com.novocode" % "junit-interface" % "0.11" % "test"
  )
}

lazy val commonJsSettings = Seq(
  Compile / doc / scalacOptions -= "-Xfatal-warnings",
  Test / parallelExecution := false
)

lazy val commonJvmSettings = Seq(
  Test / parallelExecution := false
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

lazy val plugin = project.in(file("plugin"))
  .settings(crossVersionSharedSources)
  .settings(publishSettings)
  .settings(
    name := "shapeless-plugin",
    moduleName := "shapeless-plugin",
    sbtPlugin := true,
    scalaVersion := Scala213,
    crossScalaVersions := Seq(Scala213, Scala212)
  )

lazy val core = crossProject(JSPlatform, JVMPlatform/* TODO, NativePlatform*/).crossType(CrossTypeMixed)
  .configureCross(configureJUnit)
  .settings(moduleName := "shapeless")
  .settings(coreSettings:_*)
  .configureCross(buildInfoSetup)
  .enablePlugins(SbtOsgi)
  .settings(coreOsgiSettings:_*)
  .settings(Compile / sourceGenerators += (Compile / sourceManaged).map(Boilerplate.gen).taskValue)
  .settings(mimaSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)
  /* TODO
  .nativeSettings(
    // disable scaladoc generation on native
    // currently getting errors like
    //   [error] bnd: Invalid syntax for version: ${@}, for cmd: range, arguments; [range, [==,=+), ${@}]
    Compile / packageDoc / publishArtifact := false,
    packageDoc / publishArtifact := false,
    Compile / doc / sources := Nil,
    Test / sources := Nil
  )
   */

lazy val coreJVM = core.jvm
lazy val coreJS = core.js
//TODO lazy val coreNative = core.native

lazy val scratch = crossProject(JSPlatform, JVMPlatform/* TODO, NativePlatform*/).crossType(CrossTypeMixed)
  .configureCross(configureJUnit)
  .dependsOn(core)
  .settings(moduleName := "scratch")
  .settings(coreSettings:_*)
  .settings(noPublishSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)

lazy val scratchJVM = scratch.jvm
lazy val scratchJS = scratch.js
//TODO lazy val scratchNative = scratch.native

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

lazy val examples = crossProject(JSPlatform, JVMPlatform/* TODO, NativePlatform*/).crossType(CrossTypeMixed)
  .configureCross(configureJUnit)
  .dependsOn(core)
  .settings(moduleName := "examples")
  .settings(libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.0.0")
  .settings(runAllIn(Compile))
  .settings(coreSettings:_*)
  .settings(noPublishSettings:_*)
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)
  /*
  TODO
  .nativeSettings(
    Compile / sources ~= (_.filterNot(_.getName == "sexp.scala")),
    Test / sources := Nil
  )
   */

lazy val examplesJVM = examples.jvm
lazy val examplesJS = examples.js
//TODO lazy val examplesNative = examples.native

/*
TODO
lazy val nativeTest = project
  .enablePlugins(ScalaNativePlugin)
  .settings(
    noPublishSettings,
    Compile / sourceGenerators += Def.task {
      val exclude = List(
        "StagedTypeClassExample", // scala-reflect
        "CombinatorTesting", // scala-parser-combinators
        "ALaCacheDemo" // java.util.WeakHashMap, java.util.logging.Logger
      )
      val classNames = (examplesNative / Compile / discoveredMainClasses).value.filterNot{
        c => exclude.exists(c.contains)
      }.sorted
      val src = s"""package shapeless
      |
      |object NativeMain {
      |  def main(args: Array[String]): Unit = {
      |${classNames.flatMap(cn => List(s"""println("Running $cn")""", s"$cn.main(args)")).map("    " + _).mkString("\n")}
      |  }
      |}
      |""".stripMargin
      val f = (Compile / sourceManaged).value / "shapeless" / "NativeMain.scala"
      IO.write(f, src)
      f :: Nil
    }.taskValue
  ).dependsOn(
    examplesNative
  )
*/

lazy val scalaMacroDependencies: Def.Initialize[Seq[ModuleID]] = Def.setting {
  Seq(
    scalaOrganization.value % "scala-reflect" % scalaVersion.value % "provided",
    scalaOrganization.value % "scala-compiler" % scalaVersion.value % "provided"
  )
}

lazy val crossVersionSharedSources: Seq[Setting[_]] =
  Seq(Compile, Test).map { sc =>
    (sc / unmanagedSourceDirectories) ++= {
      (sc / unmanagedSourceDirectories).value.flatMap { dir: File =>
        //Seems like cross projects don't get this source folder as well TODO: Make an issue for it
        val scala2Folder = new File(dir.getPath + "-2")

        if (dir.getName != "scala") Seq(dir)
        else CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, y)) if y >= 13 => Seq(new File(dir.getPath + "_2.13+"), scala2Folder)
          case Some((2, y)) if y <  13 => Seq(new File(dir.getPath + "_2.13-"), scala2Folder)
          case _ => Seq(dir)
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
  crossProject jvmConfigure transform jsConfigure transform /*TODO nativeConfigure transform*/
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
