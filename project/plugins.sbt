scalacOptions += "-deprecation"
libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.25"

addSbtPlugin("com.typesafe"                      % "sbt-mima-plugin"       % "0.1.14")
addSbtPlugin("com.typesafe.sbt"                  % "sbt-osgi"              % "0.9.1")
addSbtPlugin("com.eed3si9n"                      % "sbt-buildinfo"         % "0.7.0")
addSbtPlugin("com.typesafe.sbt"                  % "sbt-git"               % "0.9.3")
addSbtPlugin("org.scala-js"                      % "sbt-scalajs"           % "0.6.19")
addSbtPlugin("com.github.gseitz"                 % "sbt-release"           % "1.0.6")
addSbtPlugin("com.jsuereth"                      % "sbt-pgp"               % "1.1.0-M1")
addSbtPlugin("org.xerial.sbt"                    % "sbt-sonatype"          % "2.0")
addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings"      % "0.2.2") //https://github.com/ThoughtWorksInc/sbt-api-mappings/pull/7
addSbtPlugin("org.scoverage"                     % "sbt-scoverage"         % "1.5.0") // not updated
addSbtPlugin("org.scala-native"                  % "sbt-scala-native"      % "0.3.0") //https://github.com/scala-native/scala-native/issues/915
addSbtPlugin("org.scala-native"                  % "sbt-scalajs-crossproject" % "0.2.0") // https://github.com/scala-native/sbt-crossproject/pull/49
