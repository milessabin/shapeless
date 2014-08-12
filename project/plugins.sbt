def plugin(m: ModuleID) = Defaults.sbtPluginExtra(m, "0.13", "2.10") excludeAll ExclusionRule("org.scala-lang")

libraryDependencies ++= Seq(
  plugin("com.typesafe.sbt" % "sbt-pgp" % "0.8.1"),
  plugin("com.typesafe.sbt" % "sbt-osgi" % "0.6.0"),
  plugin("com.github.gseitz" % "sbt-release" % "0.7.1"),
  plugin("com.eed3si9n" % "sbt-buildinfo" % "0.2.5"),
  plugin("com.typesafe.sbt" % "sbt-git" % "0.6.2")
)

scalacOptions += "-deprecation"
