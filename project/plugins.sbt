resolvers += Classpaths.typesafeSnapshots

addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8.1")

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.3.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-osgi" % "0.7.0")

scalacOptions += "-deprecation"
