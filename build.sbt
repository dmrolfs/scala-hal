import Dependencies._
import BuildSettings._

name := "scalahal"

//version in ThisBuild := "0.72.1-SNAPSHOT"

organization in ThisBuild := "com.github.dmrolfs"

// dependencies
lazy val root =
  ( project in file(".") )
  .enablePlugins( BuildInfoPlugin )
  .settings(
    buildInfoKeys := Seq[BuildInfoKey]( name, version, scalaVersion, sbtVersion ),
    buildInfoPackage := "scalahal"
  )
  .settings( publish := {} )
  .aggregate( core, circe )


lazy val core = ( project in file("./core") )
  .settings( defaultSettings ++ publishSettings )

lazy val circe = ( project in file("./circe") )
  .dependsOn( core )
  .settings( defaultSettings ++ publishSettings )

scalafmtOnCompile in ThisBuild := true


publishMavenStyle in ThisBuild := true

resolvers += Resolver.url("omen bintray resolver", url("http://dl.bintray.com/omen/maven"))(Resolver.ivyStylePatterns)
