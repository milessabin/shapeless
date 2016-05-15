scalacOptions += "-deprecation"
libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.15"

addSbtPlugin("com.typesafe"                      % "sbt-mima-plugin"       % "0.1.8")
addSbtPlugin("com.typesafe.sbt"                  % "sbt-osgi"              % "0.6.0")
addSbtPlugin("com.eed3si9n"                      % "sbt-buildinfo"         % "0.6.0")
addSbtPlugin("com.typesafe.sbt"                  % "sbt-git"               % "0.8.5")
addSbtPlugin("org.scala-js"                      % "sbt-scalajs"           % "0.6.9")
addSbtPlugin("com.github.gseitz"                 % "sbt-release"           % "1.0.0")
addSbtPlugin("com.jsuereth"                      % "sbt-pgp"               % "1.0.0")
addSbtPlugin("org.xerial.sbt"                    % "sbt-sonatype"          % "1.1")
addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings"      % "0.2.2")
