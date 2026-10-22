import com.github.sbt.git.SbtGit.GitKeys.*
import sbtcrossproject.CrossProject

val Scala213 = "2.13.18"
val Scala33 = "3.3.8"
val Scala39 = "3.9.0"
val Scala310 = "3.10.0"
// The compiler the shim for the internals of Scala 3.7 and later is compiled with (see `compilerShim` below)
val Scala37 = "3.7.4"

commonSettings
noPublishSettings
crossScalaVersions := Nil

ThisBuild / organization := "com.chuusai"
ThisBuild / scalaVersion := Scala213
ThisBuild / crossScalaVersions := Seq(Scala213, Scala33, Scala39, Scala310)
ThisBuild / mimaFailOnNoPrevious := false

// GHA configuration

ThisBuild / githubWorkflowBuildPreamble := Seq(WorkflowStep.Run(List("sudo apt install clang libunwind-dev libgc-dev libre2-dev")))
// Scala 3.8 and later require JDK 17
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("8"), JavaSpec.temurin("17"))
ThisBuild / githubWorkflowBuildMatrixExclusions ++= Seq(
  MatrixExclude(Map("scala" -> Scala213, "java" -> JavaSpec.temurin("17").render)),
  MatrixExclude(Map("scala" -> Scala33, "java" -> JavaSpec.temurin("17").render)),
  MatrixExclude(Map("scala" -> Scala39, "java" -> JavaSpec.temurin("8").render)),
  MatrixExclude(Map("scala" -> Scala310, "java" -> JavaSpec.temurin("8").render))
)
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
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}",
      // A single `_3` artifact is published, built with the oldest supported Scala 3 so that it can be used with any
      // later Scala 3 version
      "CI_RELEASE" -> s"; ++$Scala213 publishSigned; ++$Scala33 publishSigned",
      "CI_SNAPSHOT_RELEASE" -> s"; ++$Scala213 publish; ++$Scala33 publish"
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

val scalacOptionsAll = List(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:higherKinds,implicitConversions"
)

def scalacOptions2(pluginJar: File) = List(
  "-Xfatal-warnings",
  "-Wconf:cat=other-implicit-type:s",
  "-Ywarn-unused:-implicits,-nowarn",
  s"-Xplugin:${pluginJar.getAbsolutePath}",
  s"-Jdummy=${pluginJar.lastModified}"
)

val scalacOptions3 = List(
  "-Werror",
  "-language:dynamics",
  "-Xmax-inlines:128",
  // Scala 2 syntax which later Scala 3 versions deprecate, but which sources shared with Scala 2 must use
  "-Wconf:msg=is deprecated for wildcard arguments of types:s",
  "-Wconf:msg=with as a type operator has been deprecated:s",
  "-Wconf:msg=is not declared infix:s",
  "-Wconf:msg=Implicit parameters should be provided with a `using` clause:s",
  "-Wconf:msg=The trailing ` _` for eta-expansion is unnecessary:s",
  "-Wconf:msg=`= _` has been deprecated:s",
  "-Wconf:msg=Ignoring \\[this\\] qualifier:s",
  "-Wconf:msg=trait App in package scala is deprecated:s"
)

def isScala2(scalaVersion: String): Boolean =
  CrossVersion.partialVersion(scalaVersion).exists(_._1 == 2)

val disableLints = List(
  "adapted-args",
  "delayedinit-select",
  "nullary-unit",
  "package-object-classes",
  "type-parameter-shadow",
  "infer-any"
)

lazy val commonSettings = versionSpecificSources ++ Seq(
  resolvers += Resolver.sonatypeCentralSnapshots,
  incOptions := incOptions.value.withLogRecompileOnMacro(false),
  scalacOptions ++= scalacOptionsAll,
  scalacOptions ++= {
    val pluginJar = (plugin / Compile / packageBin).value
    if (isScala2(scalaVersion.value)) scalacOptions2(pluginJar) else scalacOptions3
  },
  scalacOptions ++= {
    // Scala 3.10 rejects `with` as a type operator, which sources shared with Scala 2 must use
    if (CrossVersion.partialVersion(scalaVersion.value).exists { case (major, minor) => major == 3 && minor >= 10 })
      Seq("-source:3.9")
    else Nil
  },
  Compile / compile / scalacOptions ++= {
    if (isScala2(scalaVersion.value)) Seq(disableLints.iterator.map("-" + _).mkString("-Xlint:", ",", ",_"))
    else Nil
  },
  Test / scalacOptions ++= {
    // Tests summon implicits in statement position to check that they resolve
    if (isScala2(scalaVersion.value)) Nil else Seq("-Wconf:msg=pure expression does nothing in statement position:s")
  },
  Compile / doc / scalacOptions --= Seq("-Xfatal-warnings", "-Werror"),
  Compile / console / scalacOptions --= Seq("-Xfatal-warnings", "-Werror"),
  Test / console / scalacOptions --= Seq("-Xfatal-warnings", "-Werror"),
  console / initialCommands := """import shapeless._""",
  Test / parallelExecution := false,
  libraryDependencies ++= {
    if (isScala2(scalaVersion.value)) Seq(
      scalaOrganization.value % "scala-reflect" % scalaVersion.value % "provided",
      scalaOrganization.value % "scala-compiler" % scalaVersion.value % "provided"
    ) else Nil
  }
)

lazy val javaModuleName = settingKey[String]("Java module name")
lazy val jsSourceMapSettings = Def.settings(
  scalacOptions += {
    val tagOrHash =
      if (!isSnapshot.value) s"v${version.value}"
      else git.gitHeadCommit.value.getOrElse("main")
    val local = (LocalRootProject / baseDirectory).value.toURI.toString
    val remote = s"https://raw.githubusercontent.com/milessabin/shapeless/$tagOrHash/"
    val opt = if (isScala2(scalaVersion.value)) "-P:scalajs:mapSourceURI:" else "-scalajs-mapSourceURI:"
    s"$opt$local->$remote"
  }
)

def configureJUnit(crossProject: CrossProject) = crossProject
  .jvmSettings(libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % "test")
  .jsConfigure(_.enablePlugins(ScalaJSJUnitPlugin))
  .nativeConfigure(_.enablePlugins(ScalaNativeJUnitPlugin))

lazy val plugin = project.in(file("plugin"))
  .settings(publishSettings)
  .settings(
    name := "shapeless-plugin",
    moduleName := "shapeless-plugin",
    javaModuleName := "shapeless.plugin",
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    scalaVersion := Scala213,
    crossScalaVersions := Seq(Scala213)
  )

lazy val macroAnnotationSettings = Seq(
  scalacOptions ++= {
    if (isScala2(scalaVersion.value)) Seq("-Ymacro-annotations") else Nil
  }
)

lazy val coreTestMacros = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .settings(moduleName := "core-test-macros")
  .settings(commonSettings)
  .settings(noPublishSettings)
  .configureCross(buildInfoSetup)
  .settings(macroAnnotationSettings)

/**
 * The parts of the Scala 3 implementation which depend on compiler internals (see `shapeless.CompilerShim`).
 *
 * Compiler internals aren't binary compatible between Scala 3 versions, so the same source is compiled against a
 * compiler from each range of versions with compatible internals, into a package for that range, and the classes are
 * included with the shapeless classes for Scala 3, which select the one for the running compiler. Only the class files
 * are included, so that compilers older than the one a shim is compiled with never read its TASTy.
 *
 * The sources in `compiler-shims/src` are common to all shims. Those in `compiler-shims/src-<specifics>` are specific
 * to the ranges of versions whose internals are source compatible with those of `<specifics>`.
 *
 * Shims are compiled with compilers which target Java 8, so that they can be loaded by any compiler of their range,
 * whichever Java version it runs on.
 */
def compilerShim(variant: String, compilerVersion: String, specifics: String) =
  Project(s"compilerShim${variant.drop(1).replace("_", "")}", file("compiler-shims") / variant)
    .settings(noPublishSettings)
    .settings(
      scalaVersion := compilerVersion,
      crossScalaVersions := Seq(compilerVersion),
      scalacOptions ++= scalacOptionsAll ++ Seq("-Werror"),
      libraryDependencies += "org.scala-lang" %% "scala3-compiler" % compilerVersion % Provided,
      Compile / unmanagedSources +=
        (LocalRootProject / baseDirectory).value / "core" / "shared" / "src" / "main" / "scala-3" / "shapeless" / "compilershim.scala",
      Compile / sourceGenerators += Def.task {
        val shims = (LocalRootProject / baseDirectory).value / "compiler-shims"
        val pkg = "package shapeless.compilershims"
        ((shims / "src" * "*.scala").get ++ (shims / s"src-$specifics" * "*.scala").get).map { template =>
          val out = (Compile / sourceManaged).value / template.getName
          IO.write(out, IO.read(template).replace(s"$pkg\n", s"$pkg.$variant\n"))
          out
        }
      }.taskValue
    )

lazy val compilerShim33 = compilerShim("v3_3", Scala33, "3.3")
lazy val compilerShim37 = compilerShim("v3_7", Scala37, "3.7")
lazy val compilerShims = Seq(compilerShim33, compilerShim37)

lazy val compilerShimClasses = taskKey[Seq[File]]("The class files of the compiler shims, for Scala 3")

lazy val compilerShimSettings = Seq(
  compilerShimClasses := Def.taskDyn {
    if (isScala2(scalaVersion.value)) Def.task(Seq.empty[File])
    else Def.task {
      val out = target.value / "compiler-shims"
      val classDirs = Seq((compilerShim33 / Compile / classDirectory).value, (compilerShim37 / Compile / classDirectory).value)
      val _ = ((compilerShim33 / Compile / compile).value, (compilerShim37 / Compile / compile).value)
      val classes = classDirs.flatMap { dir =>
        (dir / "shapeless" / "compilershims" ** "*.class").get.flatMap(f => f.relativeTo(dir).map(f -> out / _.getPath))
      }
      IO.delete(out)
      IO.copy(classes)
      Seq(out)
    }
  }.value,
  Compile / products ++= compilerShimClasses.value
)

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .configureCross(configureJUnit)
  .settings(moduleName := "shapeless", javaModuleName := "shapeless")
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
  .settings(compilerShimSettings)
  .jvmSettings(scala3GeneratedSettings(Test))

lazy val coreJVM = core.jvm
lazy val coreJS = core.js.settings(jsSourceMapSettings)
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
  .settings(libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.5.0")
  .settings(runAllIn(Compile))
  .settings(commonSettings)
  .settings(noPublishSettings)
  .nativeSettings(Compile / sources ~= (_.filterNot(_.getName == "sexp.scala")))
  .jvmSettings(scala3GeneratedSettings(Compile, Test))

lazy val examplesJVM = examples.jvm
lazy val examplesJS = examples.js
lazy val examplesNative = examples.native

/**
 * Sources which differ between Scala versions.
 *
 * Besides the shared `scala` directories and the `scala-2`/`scala-3` directories sbt adds for the major Scala version,
 * sources may be placed in `scala-<major>.<minor>` directories (eg. `scala-3.3`). A source in a more version-specific
 * directory replaces any source with the same path relative to a less specific directory, so `scala-3/X.scala` is
 * compiled instead of `scala/X.scala` with Scala 3, and `scala-3.3/X.scala` instead of either with Scala 3.3. A marker
 * file `X.scala.omit` (whose content, typically the reason, is ignored) replaces `X.scala` with nothing.
 *
 * This lets a test or example which must differ on Scala 3 be written separately, leaving the version compiled with
 * Scala 2 unchanged.
 */
lazy val versionSpecificSources: Seq[Setting[?]] =
  Seq(Compile, Test).flatMap { sc =>
    Seq(
      sc / unmanagedSourceDirectories := {
        val dirs = (sc / unmanagedSourceDirectories).value
        val minorDirs = CrossVersion.partialVersion(scalaVersion.value).toList.flatMap { case (major, minor) =>
          dirs.filter(_.getName == "scala").map(dir => dir.getParentFile / s"scala-$major.$minor")
        }
        (dirs ++ minorDirs).distinct
      },
      sc / unmanagedSources := {
        val dirs = (sc / unmanagedSourceDirectories).value
        def specificity(dir: File): Int = dir.getName.split("-", 2) match {
          case Array("scala", version) => if (version.contains(".")) 2 else 1
          case _ => 0
        }
        def locate(file: File, path: String => String): (File, (String, Int)) = {
          val located = dirs.flatMap(dir => file.relativeTo(dir).map(rel => (path(rel.getPath), specificity(dir))))
          (file, located.headOption.getOrElse((path(file.getPath), 0)))
        }
        val sources = (sc / unmanagedSources).value.map(locate(_, identity))
        val omissions = dirs.flatMap(dir => (dir ** "*.scala.omit").get).map(locate(_, _.stripSuffix(".omit")))
        val selected = (sources ++ omissions).groupBy(_._2._1).values.flatMap { variants =>
          val max = variants.map(_._2._2).max
          variants.filter(_._2._2 == max).map(_._1)
        }.toSet
        sources.map(_._1).filter(selected)
      }
    )
  }

/**
 * Scala 3 versions of tests and examples generated from their Scala 2 versions by the scalafix rules in `scalafix`.
 *
 * A generated `scala-3/X.scala` starts with the line `Scala3Generate.header`, and is compiled with Scala 3 in place of
 * `scala/X.scala` as described for `versionSpecificSources` above. It's what the rules generate from `scala/X.scala`,
 * with the patch `scala-3/X.scala.patch`, if any, applied, which makes the changes the rules can't. The rules use the
 * SemanticDB of the Scala 2 versions, so these are run with Scala 2,
 *
 * - `scala3Generate` regenerates the generated sources, and `scala3Generate <path of scala/X.scala> ...` also
 *   generates the Scala 3 versions of the sources given.
 * - `scala3RecordPatches` records the differences between the generated sources, as edited, and what the rules
 *   generate as their patches.
 * - `scala3GenerateCheck` fails if any generated source differs from what `scala3Generate` would generate.
 */
lazy val scala3Generate = inputKey[Unit]("Generate the Scala 3 versions of sources from their Scala 2 versions")
lazy val scala3GenerateCheck = taskKey[Unit]("Check that the generated Scala 3 versions of sources are up to date")
lazy val scala3RecordPatches = taskKey[Unit]("Record the edits of generated Scala 3 versions of sources as patches")
lazy val scala3GenerateInputs = taskKey[Scala3Generate.Inputs]("The inputs to scalafix for a configuration")
lazy val scala3GenerateSources = inputKey[Seq[Scala3Generate.Generated]]("The Scala 3 versions the rules generate")

def scala3GeneratedSettings(configs: Configuration*): Seq[Setting[?]] = {
  val filter = ScopeFilter(configurations = inConfigurations(configs: _*))
  configs.map { sc =>
    sc / scala3GenerateInputs := {
      if (!isScala2(scalaVersion.value)) sys.error(s"Scala 3 versions are generated with Scala 2 (eg. ++$Scala213)")
      val _ = (sc / compile).value
      Scala3Generate.Inputs(
        (sc / unmanagedSourceDirectories).value,
        (sc / fullClasspath).value.files,
        (sc / semanticdbTargetRoot).value,
        (sc / scalacOptions).value
      )
    }
  } ++ Seq(
    semanticdbEnabled := isScala2(scalaVersion.value),
    semanticdbVersion := "4.15.2",
    // The rules use the implicit conversions which the SemanticDB records as synthetics
    semanticdbOptions ++= (if (isScala2(scalaVersion.value)) Seq("-P:semanticdb:synthetics:on") else Nil),
    scala3GenerateSources := {
      val requested = Def.spaceDelimited("<path of scala/X.scala>").parsed.map(file(_).getAbsoluteFile)
      val rulesClasspath = (scalafixRules / Compile / exportedProductJars).value.files
      val sourceroot = (LocalRootProject / baseDirectory).value
      val log = streams.value.log
      scala3GenerateInputs.all(filter).value.flatMap { inputs =>
        Scala3Generate.generate(Scala3Generate.sources(inputs, requested), inputs, rulesClasspath, sourceroot, scalaVersion.value, log)
      }
    },
    scala3Generate := {
      val log = streams.value.log
      val generated = scala3GenerateSources.evaluated
      val failures = generated.flatMap(g => g.patched.left.toOption)
      if (failures.nonEmpty) sys.error(failures.mkString("\n"))
      generated.foreach { g =>
        g.patched.foreach { content =>
          if (!g.target.exists || IO.read(g.target) != content) {
            IO.write(g.target, content)
            log.info(s"Generated ${g.target}")
          }
        }
      }
    },
    scala3RecordPatches := {
      val log = streams.value.log
      scala3GenerateSources.toTask("").value.foreach { g =>
        g.patchFor(IO.read(g.target)) match {
          case Some(patch) =>
            if (!g.patchFile.exists || IO.read(g.patchFile) != patch) {
              IO.write(g.patchFile, patch)
              log.info(s"Recorded ${g.patchFile}")
            }
          case None =>
            if (g.patchFile.exists) {
              IO.delete(g.patchFile)
              log.info(s"Deleted ${g.patchFile}")
            }
        }
      }
    },
    scala3GenerateCheck := {
      val problems = scala3GenerateSources.toTask("").value.flatMap { g =>
        g.patched match {
          case Left(failure) => Some(failure)
          case Right(content) if !g.target.exists || IO.read(g.target) != content =>
            Some(s"${g.target} is out of date (run scala3Generate, or scala3RecordPatches if it was edited)")
          case Right(_) => None
        }
      }
      if (problems.nonEmpty) sys.error(problems.mkString("\n"))
    }
  )
}

/** The scalafix rules which generate Scala 3 versions of sources from their Scala 2 versions */
lazy val scalafixRules = project.in(file("scalafix"))
  .settings(noPublishSettings)
  .settings(
    moduleName := "shapeless-scalafix",
    scalaVersion := Scala213,
    crossScalaVersions := Seq(Scala213),
    libraryDependencies += "ch.epfl.scala" %% "scalafix-core" % "0.14.6"
  )

lazy val publishSettings = Seq(
  // See the CI_RELEASE command above: only the Scala 3.3 build of the `_3` artifacts is published
  publish / skip := Set(Scala39, Scala310).contains(scalaVersion.value),
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
    if (isScala2(scalaVersion.value)) {
      val Some((major, minor)) = CrossVersion.partialVersion(scalaVersion.value)
      Seq(s"""!scala.quasiquotes,scala.*;version="[$major.$minor,$major.${minor+1})"""")
    } else Seq("""scala.*;version="[2.13,4)"""")
  },
  OsgiKeys.additionalHeaders := Map("-removeheaders" -> "Include-Resource,Private-Package")
)
