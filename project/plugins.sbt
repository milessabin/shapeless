resolvers += Classpaths.typesafeSnapshots

addSbtPlugin("com.jsuereth" % "xsbt-gpg-plugin" % "0.6")

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.1.0")

scalacOptions += "-deprecation"

addSbtPlugin("io.spray" % "sbt-boilerplate" % "0.5.0")
