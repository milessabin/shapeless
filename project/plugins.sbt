scalacOptions += "-deprecation"
libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.30"

val scalaJsVersion = Option(System.getenv("SCALA_JS_VERSION")).getOrElse("1.0.1")

addSbtPlugin("com.typesafe"                      % "sbt-mima-plugin"       % "0.7.0")
addSbtPlugin("com.typesafe.sbt"                  % "sbt-osgi"              % "0.9.5")
addSbtPlugin("com.eed3si9n"                      % "sbt-buildinfo"         % "0.9.0")
addSbtPlugin("com.geirsson"                      % "sbt-ci-release"        % "1.5.3")
addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings"      % "3.0.0")
addSbtPlugin("org.scoverage"                     % "sbt-scoverage"         % "1.6.1")
addSbtPlugin("org.scala-js"                      % "sbt-scalajs"           % scalaJsVersion)
addSbtPlugin("org.scala-native"                  % "sbt-scala-native"      % "0.4.0-M2")
addSbtPlugin("org.portable-scala"                % "sbt-scalajs-crossproject" % "1.0.0")
addSbtPlugin("org.portable-scala"                % "sbt-scala-native-crossproject" % "1.0.0")
