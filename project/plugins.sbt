resolvers += Classpaths.typesafeSnapshots

addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8.1")

addSbtPlugin("com.typesafe.sbt" % "sbt-osgi" % "0.6.0")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.7.1")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.2.5")

addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.6.2")

scalacOptions += "-deprecation"
