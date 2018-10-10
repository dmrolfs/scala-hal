import sbt.Keys._
import sbt._

// import spray.revolver.RevolverPlugin._

object BuildSettings {
  val VERSION = "0.1.0"

  val scalacBuildOptions = Seq(
    // "-encoding",
    // "utf8",
    "-target:jvm-1.8",
    "-language:experimental.macros",
    "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
//    "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
    "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
    "-encoding", "utf-8",                // Specify character encoding used by source files.
    "-explaintypes",                     // Explain type errors in more detail.
    "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
    "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
    "-Xlint:by-name-right-associative",  // By-name parameter of right associative operator.
    "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
    "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
    "-Xlint:option-implicit",            // Option.apply used implicit view.
    "-Xlint:package-object-classes",     // Class or object defined in package object.
    "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
    "-Xlint:unsound-match",              // Pattern match may not be typesafe.
    "-Yno-adapted-args",                 // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
    "-Ypartial-unification",             // Enable partial unification in type constructor inference
    "-Ywarn-dead-code",                  // Warn when dead code is identified.
    "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
    "-Ywarn-inaccessible",               // Warn about inaccessible types in method signatures.
    "-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
    "-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
    "-Ywarn-numeric-widen",              // Warn when numerics are widened.
    "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
//    "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
    "-Ywarn-unused:locals",              // Warn if a local definition is unused.
    "-Ywarn-unused:params",              // Warn if a value parameter is unused.
    "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
    "-Ywarn-unused:privates",            // Warn if a private member is unused.
//    "-Ywarn-value-discard",              // Warn when non-Unit expression results are unused.
//      "-Ylog-classpath",
    // "-Xlog-implicits",
    // "-Ymacro-debug-verbose",
    // "-Ywarn-adapted-args",
//      "-Xfatal-warnings",
    "-Xlog-reflective-calls"
  )

//  def projSettings( dependencies: Seq[ModuleID] = Seq.empty[ModuleID] ) = {
//    defaultSettings ++ Seq(
//      libraryDependencies ++= dependencies
//    )
//  }

  lazy val defaultSettings = Defaults.coreDefaultSettings ++ /*Format.settings ++*/ Seq(
    version := VERSION,
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
    crossScalaVersions := Seq( "2.12.6" ),
    scalaVersion := crossScalaVersions{ (vs: Seq[String]) => vs.head }.value,
    // updateOptions := updateOptions.value.withCachedResolution(true),
    scalacOptions ++= scalacBuildOptions,
    javacOptions ++= Seq(
      "-source", "1.8",
      "-target", "1.8"
    ),
    javaOptions ++= Seq(
      "-Dconfig.trace=loads"
    ),
    // licenses := Seq("Apache 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.html")),
    conflictManager := ConflictManager.latestRevision,
    dependencyOverrides := Dependencies.defaultDependencyOverrides,

    resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
    resolvers += "omen-bintray" at "http://dl.bintray.com/omen/maven",
    resolvers += "Typesafe releases" at "http://repo.typesafe.com/typesafe/releases",
    resolvers += "eaio releases" at "http://eaio.com/maven2",
    resolvers += "Sonatype OSS Releases"  at "http://oss.sonatype.org/content/repositories/releases/",
//    resolvers += "Numerical Method's Repository" at "http://repo.numericalmethod.com/maven/",  // don't want to use due to $$$
    resolvers += Resolver.jcenterRepo,
    resolvers += Resolver.sonatypeRepo( "snapshots" ),
    resolvers += Classpaths.sbtPluginReleases,
    resolvers += "OSS JFrog Artifactory" at "http://oss.jfrog.org/artifactory/oss-snapshot-local",

    // SLF4J initializes itself upon the first logging call.  Because sbt
    // runs tests in parallel it is likely that a second thread will
    // invoke a second logging call before SLF4J has completed
    // initialization from the first thread's logging call, leading to
    // these messages:
    //   SLF4J: The following loggers will not work because they were created
    //   SLF4J: during the default configuration phase of the underlying logging system.
    //   SLF4J: See also http://www.slf4j.org/codes.html#substituteLogger
    //   SLF4J: com.imageworks.common.concurrent.SingleThreadInfiniteLoopRunner
    //
    // As a workaround, load SLF4J's root logger before starting the unit
    // tests [1].
    //
    // [1] http://stackoverflow.com/a/12095245
    testOptions in Test += Tests.Setup( classLoader =>
      classLoader
        .loadClass( "org.slf4j.LoggerFactory" )
        .getMethod( "getLogger", classLoader.loadClass("java.lang.String") )
        .invoke( null, "ROOT" )
    ),
    parallelExecution in Test := false,
    testOptions in Test += Tests.Argument( TestFrameworks.ScalaTest, "-oDFT" ),
    triggeredMessage in ThisBuild := Watched.clearWhenTriggered,
    cancelable in Global := true
  )


  def publishSettings: Seq[Setting[_]] = {
    if ( VERSION.endsWith("-SNAPSHOT") ) {
      println( "PUBLISH_MODULE -- SNAPSHOT" )
      Seq(
        publishTo := Some("Artifactory Realm" at "http://oss.jfrog.org/artifactory/oss-snapshot-local"),
        credentials := List(Path.userHome / ".bintray" / ".jfrog-oss").filter(_.exists).map(Credentials(_))
      )
//      publishMavenStyle := true
      // Only setting the credentials file if it exists (#52)
    } else {
      println( "PUBLISH_MODULE" )
      Seq(
        publishTo := Some("Bintray API Realm" at "http://api.bintray.com"),
        credentials := List(Path.userHome / ".bintray" / ".credentials").filter(_.exists).map(Credentials(_)),
        pomExtra := {
          <scm>
            <url>https://github.com</url>
            <connection>https://github.com/dmrolfs/scala-hal.git</connection>
          </scm>
          <developers>
            <developer>
              <id>dmrolfs</id>
              <name>Damon Rolfs</name>
              <url>http://dmrolfs.github.io/</url>
            </developer>
          </developers>
        }
      )
//      publishMavenStyle := true
//      resolvers += Resolver.url("omen bintray resolver", url("http://dl.bintray.com/omen/maven"))(Resolver.ivyStylePatterns)
//      licenses := ("MIT", url("http://opensource.org/licenses/MIT")) :: Nil // this is required! otherwise Bintray will reject the code
    }
  }

}
