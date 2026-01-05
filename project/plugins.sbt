scalacOptions += "-deprecation"
libraryDependencies += "org.slf4j" % "slf4j-nop" % "2.0.17"

addSbtPlugin("com.typesafe"                      % "sbt-mima-plugin"       % "1.1.4")
addSbtPlugin("com.github.sbt"                    % "sbt-osgi"              % "0.10.0")
addSbtPlugin("com.eed3si9n"                      % "sbt-buildinfo"         % "0.13.1")
addSbtPlugin("com.github.sbt"                    % "sbt-ci-release"        % "1.11.2")
addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings"      % "3.0.2")
addSbtPlugin("org.scala-js"                      % "sbt-scalajs"           % "1.20.2")
addSbtPlugin("org.scala-native"                  % "sbt-scala-native"      % "0.5.9")
addSbtPlugin("org.portable-scala"                % "sbt-scalajs-crossproject" % "1.3.2")
addSbtPlugin("org.portable-scala"                % "sbt-scala-native-crossproject" % "1.3.2")
addSbtPlugin("com.github.sbt"                    % "sbt-github-actions"    % "0.29.0")
