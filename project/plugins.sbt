resolvers += Classpaths.typesafeSnapshots

addSbtPlugin("com.jsuereth" % "xsbt-gpg-plugin" % "0.6", sbtVersion = "0.12.0-Beta2")

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.1.0-SNAPSHOT", sbtVersion = "0.12.0-Beta2")

scalacOptions += "-deprecation"
