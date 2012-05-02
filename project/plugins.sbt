resolvers += Classpaths.typesafeSnapshots

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.1.0-SNAPSHOT", sbtVersion = "0.12.0-Beta2")

scalacOptions += "-deprecation"