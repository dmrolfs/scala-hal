import BuildSettings._
import Dependencies._
import sbtassembly.AssemblyPlugin.autoImport.MergeStrategy

name := "scalahal-core"

description := "lorem ipsum."

scalacOptions := scalacBuildOptions

resolvers += "omen-bintray" at "http://dl.bintray.com/omen/maven"

libraryDependencies ++=
  commonDependencies ++
  circe.all ++
  cats.all ++
  Seq( uritemplate )

testOptions in Test += Tests.Argument( "-oDF" )

assemblyOption in assembly := (assemblyOption in assembly).value.copy(includeScala = false, includeDependency = false)

assemblyJarName in assembly := s"${organizationName.value}-${name.value}-${version.value}.jar"
