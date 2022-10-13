scalacOptions += "-deprecation"
libraryDependencies += "org.slf4j" % "slf4j-nop" % "2.0.3"

addSbtPlugin("com.typesafe"                      % "sbt-mima-plugin"       % "1.1.1")
addSbtPlugin("com.typesafe.sbt"                  % "sbt-osgi"              % "0.9.6")
addSbtPlugin("com.eed3si9n"                      % "sbt-buildinfo"         % "0.11.0")
addSbtPlugin("com.geirsson"                      % "sbt-ci-release"        % "1.5.7")
addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings"      % "3.0.2")
addSbtPlugin("org.scala-js"                      % "sbt-scalajs"           % "1.11.0")
addSbtPlugin("org.scala-native"                  % "sbt-scala-native"      % "0.4.7")
addSbtPlugin("org.portable-scala"                % "sbt-scalajs-crossproject" % "1.2.0")
addSbtPlugin("org.portable-scala"                % "sbt-scala-native-crossproject" % "1.2.0")
addSbtPlugin("com.codecommit"                    % "sbt-github-actions"    % "0.10.1")
