scalacOptions += "-deprecation"
libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.30"

addSbtPlugin("com.typesafe"                      % "sbt-mima-plugin"       % "0.9.1")
addSbtPlugin("com.typesafe.sbt"                  % "sbt-osgi"              % "0.9.6")
addSbtPlugin("com.eed3si9n"                      % "sbt-buildinfo"         % "0.10.0")
addSbtPlugin("com.geirsson"                      % "sbt-ci-release"        % "1.5.7")
addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings"      % "3.0.0")
addSbtPlugin("org.scala-js"                      % "sbt-scalajs"           % "1.5.1")
addSbtPlugin("org.scala-native"                  % "sbt-scala-native"      % "0.4.0")
addSbtPlugin("org.portable-scala"                % "sbt-scalajs-crossproject" % "1.0.0")
addSbtPlugin("org.portable-scala"                % "sbt-scala-native-crossproject" % "1.0.0")
addSbtPlugin("com.codecommit"                    % "sbt-github-actions"    % "0.11.0")
