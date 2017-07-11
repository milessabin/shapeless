scalacOptions += "-deprecation"
libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.25"

addSbtPlugin("com.typesafe"                      % "sbt-mima-plugin"       % "0.1.14")
addSbtPlugin("com.typesafe.sbt"                  % "sbt-osgi"              % "0.9.1")
addSbtPlugin("com.eed3si9n"                      % "sbt-buildinfo"         % "0.7.0")
addSbtPlugin("com.typesafe.sbt"                  % "sbt-git"               % "0.9.2")
addSbtPlugin("org.scala-js"                      % "sbt-scalajs"           % "0.6.16")
addSbtPlugin("com.github.gseitz"                 % "sbt-release"           % "1.0.4")
addSbtPlugin("com.jsuereth"                      % "sbt-pgp"               % "1.0.1")
addSbtPlugin("org.xerial.sbt"                    % "sbt-sonatype"          % "1.1")
addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings"      % "0.2.2")
addSbtPlugin("org.scoverage"                     % "sbt-scoverage"         % "1.5.0")
addSbtPlugin("org.scala-native"                  % "sbt-scala-native"      % "0.3.1")
addSbtPlugin("org.scala-native"                  % "sbt-scalajs-crossproject" % "0.2.0")
