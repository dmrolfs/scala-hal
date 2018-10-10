// see: https://github.com/sbt/sbt-git#known-issues
libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.25"

logLevel := Level.Warn

addSbtPlugin( "com.eed3si9n" % "sbt-buildinfo" % "0.8.0" )

addSbtPlugin("com.eed3si9n"                      % "sbt-assembly"           % "0.14.6")

addSbtPlugin("org.foundweekends"                 % "sbt-bintray"            % "0.5.4")

addSbtPlugin("com.geirsson" % "sbt-scalafmt" % "1.5.1")
addSbtPlugin("org.scalastyle"                    %% "scalastyle-sbt-plugin" % "1.0.0")

addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings"       % "2.0.2")

addSbtPlugin("com.timushev.sbt"                  % "sbt-updates"            % "0.3.4")
addSbtPlugin("com.typesafe.sbt"                  % "sbt-git"                % "0.9.3")
addSbtPlugin("net.virtual-void"                  % "sbt-dependency-graph"   % "0.9.0")
addSbtPlugin("io.get-coursier"                   % "sbt-coursier"           % "1.0.1")

addSbtPlugin("org.scoverage"                     % "sbt-scoverage"          % "1.5.1")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
