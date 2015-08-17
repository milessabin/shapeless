/*
 * Copyright (c) 2011-14 Miles Sabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import sbt._
import Keys._

import com.typesafe.sbt.osgi.SbtOsgi._

import sbtbuildinfo.Plugin._

import com.typesafe.sbt.SbtGit._
import GitKeys._

import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import com.typesafe.tools.mima.plugin.MimaKeys
import MimaKeys.{previousArtifact, binaryIssueFilters}

object ShapelessBuild extends Build {

  lazy val shapeless = (project in file(".")
    aggregate (core, examples)
    dependsOn (core, examples, scratch)
    settings (commonSettings: _*)
    settings (
      moduleName := "shapeless-root",

      (unmanagedSourceDirectories in Compile) := Nil,
      (unmanagedSourceDirectories in Test) := Nil,

      publish := (),
      publishLocal := (),

      // Add back mima-report-binary-issues once 2.3.0 final is released
      addCommandAlias("validate", ";test;doc")
    )
  )

  lazy val core = (project
      settings(
        commonSettings ++ Publishing.settings ++ osgiSettings ++
        buildInfoSettings ++ mimaDefaultSettings: _*
      )
      settings(
        moduleName := "shapeless",

        managedSourceDirectories in Test := Nil,

        libraryDependencies ++= Seq(
          "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
          "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
          "com.novocode" % "junit-interface" % "0.7" % "test"
        ),

        (sourceGenerators in Compile) <+= (sourceManaged in Compile) map Boilerplate.gen,
        (sourceGenerators in Compile) <+= buildInfo,

        mappings in (Compile, packageSrc) <++=
          (sourceManaged in Compile, managedSources in Compile) map { (base, srcs) =>
            (srcs pair (Path.relativeTo(base) | Path.flat))
          },

        mappings in (Compile, packageSrc) <++=
          (mappings in (Compile, packageSrc) in LocalProject("examples")),

        previousArtifact := {
          val Some((major, minor)) = CrossVersion.partialVersion(scalaVersion.value)
          if (major == 2 && minor == 11)
            Some(organization.value %% moduleName.value % "2.3.0")
          else
            None
        },

        binaryIssueFilters ++= {
          import com.typesafe.tools.mima.core._
          import com.typesafe.tools.mima.core.ProblemFilters._

          // Filtering the methods that were added since the checked version
          // (these only break forward compatibility, not the backward one)
          Seq(
            ProblemFilters.exclude[MissingMethodProblem]("shapeless.ops.hlist#LowPriorityRotateLeft.hlistRotateLeft"),
            ProblemFilters.exclude[MissingMethodProblem]("shapeless.ops.hlist#LowPriorityRotateRight.hlistRotateRight"),
            ProblemFilters.exclude[MissingMethodProblem]("shapeless.ops.coproduct#LowPriorityRotateLeft.coproductRotateLeft"),
            ProblemFilters.exclude[MissingMethodProblem]("shapeless.ops.coproduct#LowPriorityRotateRight.coproductRotateRight"),
            ProblemFilters.exclude[IncompatibleResultTypeProblem]("shapeless.GenericMacros.shapeless$GenericMacros$$mkCoproductCases$1"),
            ProblemFilters.exclude[MissingMethodProblem]("shapeless.Generic1Macros.shapeless$Generic1Macros$$mkCoproductCases$1"),
            ProblemFilters.exclude[MissingMethodProblem]("shapeless.SingletonTypeUtils.isValueClass"),
            ProblemFilters.exclude[MissingMethodProblem]("shapeless.CaseClassMacros.mkHListTypTree"),
            ProblemFilters.exclude[MissingMethodProblem]("shapeless.CaseClassMacros.reprTypTree"),
            ProblemFilters.exclude[MissingMethodProblem]("shapeless.CaseClassMacros.mkCompoundTypTree"),
            ProblemFilters.exclude[MissingMethodProblem]("shapeless.CaseClassMacros.mkCoproductTypTree"),
            ProblemFilters.exclude[MissingMethodProblem]("shapeless.CaseClassMacros.isAnonOrRefinement"),
            ProblemFilters.exclude[MissingMethodProblem]("shapeless.CaseClassMacros.mkTypTree"),
            ProblemFilters.exclude[MissingMethodProblem]("shapeless.CaseClassMacros.isAccessible"),
            ProblemFilters.exclude[IncompatibleMethTypeProblem]("shapeless.ProductMacros.mkProductImpl"),
            ProblemFilters.exclude[IncompatibleMethTypeProblem]("shapeless.ProductMacros.forward")
          )
        },

        OsgiKeys.exportPackage := Seq("shapeless.*;version=${Bundle-Version}"),
        OsgiKeys.importPackage := Seq("""scala.*;version="$<range;[==,=+);$<@>>""""),
        OsgiKeys.additionalHeaders := Map("-removeheaders" -> "Include-Resource,Private-Package"),

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
    )

  lazy val scratch = (project
    dependsOn core
    settings (commonSettings: _*)
    settings (
      moduleName := "shapeless-scratch",

      libraryDependencies ++= Seq(
        // needs compiler for `scala.tools.reflect.Eval`
        "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
        "com.novocode" % "junit-interface" % "0.7" % "test"
      ),

      publish := (),
      publishLocal := ()
    )
  )

  lazy val examples = (project
    dependsOn core
    settings (commonSettings: _*)
    settings (
      moduleName := "shapeless-examples",

      libraryDependencies ++= Seq(
        // needs compiler for `scala.tools.reflect.Eval`
        "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
        "com.novocode" % "junit-interface" % "0.7" % "test"
      ),

      runAllIn(Compile),

      publish := (),
      publishLocal := ()
    )
  )

  lazy val runAll = TaskKey[Unit]("run-all")

  def runAllIn(config: Configuration) = {
    runAll in config <<= (discoveredMainClasses in config, runner in run, fullClasspath in config, streams) map {
      (classes, runner, cp, s) => classes.foreach(c => runner.run(c, Attributed.data(cp), Seq(), s.log))
    }
  }

  def commonSettings =
    Seq(
      organization        := "com.chuusai",
      scalaVersion        := "2.11.7",
      crossScalaVersions  := Seq("2.11.7", "2.12.0-M2"),

      (unmanagedSourceDirectories in Compile) <<= (scalaSource in Compile)(Seq(_)),
      (unmanagedSourceDirectories in Test) <<= (scalaSource in Test)(Seq(_)),

      scalacOptions       := Seq(
        "-feature",
        "-language:higherKinds",
        "-language:implicitConversions",
        "-Xfatal-warnings",
        "-deprecation",
        "-unchecked"),

      initialCommands in console := """import shapeless._"""
    )
}
