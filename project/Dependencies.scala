import sbt.Keys._
import sbt._

object Dependencies {
  object Scope {
    def compile( deps: ModuleID* ): Seq[ModuleID] = deps map ( _ % "compile" )
    def provided( deps: ModuleID* ): Seq[ModuleID] = deps map ( _ % "provided" )
    def test( deps: ModuleID* ): Seq[ModuleID] = deps map ( _ % "test" )
    def runtime( deps: ModuleID* ): Seq[ModuleID] = deps map ( _ % "runtime" )
    def container( deps: ModuleID* ): Seq[ModuleID] = deps map ( _ % "container" )
  }

  trait Module {
    def groupId: String
    def version: String 
    def artifactId( id: String ): String
    def isScala: Boolean = true
    def module( id: String ): ModuleID = {
      if ( isScala ) groupId %% artifactId(id) % version 
      else groupId % artifactId(id) % version
    }
  }

  trait SimpleModule extends Module {
    def artifactIdRoot: String
    override def artifactId( id: String ): String = if ( id.isEmpty ) artifactIdRoot else s"$artifactIdRoot-$id"
  }


  object silencer {
    private val version = "1.2"

    val all = Seq(
      compilerPlugin("com.github.ghik" %% "silencer-plugin" % version),
      "com.github.ghik" %% "silencer-lib" % version
    )
  }

  val parserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"

  object akka extends SimpleModule {
    override val groupId = "com.typesafe.akka"
    override val artifactIdRoot = "akka"
    override val version = "2.5.16"
    def all: Seq[ModuleID] = Seq( actor, stream, agent, cluster, clusterSharding, contrib, persistence, remote, slf4j )

    val actor = module( "actor" ) withSources() withJavadoc()
    val stream = module( "stream" )
    val agent = module( "agent" )
    val cluster = module( "cluster" )
    val clusterSharding = module( "cluster-sharding" )
    val clusterMetrics = module( "cluster-metrics" )
    val clusterTools = module( "cluster-tools" )
    val contrib = module( "contrib" )
    val persistence = module( "persistence" )
    val persistenceQuery = module( "persistence-query" )
    val remote = module( "remote" )
    val slf4j = module( "slf4j" )
    val testkit = module( "testkit" )
    val streamsTestkit = module( "stream-testkit" )

    val kryo = "com.github.romix.akka" %% "akka-kryo-serialization" % "0.5.2"
    val kryoSerializers = "de.javakaffee" % "kryo-serializers" % "0.41"
  }

  object persistence {
    val cassandra = "com.typesafe.akka" %% "akka-persistence-cassandra" % "0.89"
    val leveldb = "org.iq80.leveldb" % "leveldb" % "0.9" // "org.iq80.leveldb" % "leveldb" % "0.9"
    val leveldbjni = "org.fusesource.leveldbjni" % "leveldbjni-all" % "1.8" // "org.fusesource.leveldbjni" % "leveldbjni-all" % "1.8"
  }

  object cats extends SimpleModule {
    override val groupId = "org.typelevel"
    override val artifactIdRoot = "cats"
    override val version = "1.4.0"

    val core = module( "core" )
    val kernel = module( "kernel" )
    val macros = module( "macros" )

    def all = Seq( core, kernel, macros )
  }

  object monix extends SimpleModule {
    override val groupId = "io.monix"
    override val artifactIdRoot = "monix"
    override val version = "2.3.3"

    val core = module( "" )
    val cats = module( "cats" )

    def all = Seq( core, cats )
  }

  object time {
    val joda = "joda-time" % "joda-time" % "2.10"
    val jodaConvert = "org.joda" % "joda-convert" % "2.1.1"
    val scalaTime = "com.github.nscala-time" %% "nscala-time" % "2.20.0"
    def all = Seq( joda, jodaConvert, scalaTime )
  }

  object logging {
    object logback extends SimpleModule {
      override val groupId = "ch.qos.logback"
      override val artifactIdRoot = "logback"
      override val version = "1.2.3"
      override val isScala = false
      val core = module( "core" )
      val classic = module( "classic" )
    }

    val journal = "io.verizon.journal" %% "core" % "3.0.19"
    val slf4j = "org.slf4j" % "slf4j-api" % "1.7.25" intransitive
    val log4jOverSlf4j = "org.slf4j" % "log4j-over-slf4j" % "1.7.25"

    def all = Scope.provided( logback.core, logback.classic, slf4j, log4jOverSlf4j ) :+ journal
  }

  object metrics extends SimpleModule {
    override val groupId = "io.dropwizard.metrics"
    override val artifactIdRoot = "metrics"
    override val version = "4.0.3"
    override val isScala = false

    def all = Seq( core, graphite ) ++ scala.all // ++ kamon.all

    val core = module( "core" )
    val graphite = module( "graphite" )

    val hdrhistogramReservoir = "org.mpierce.metrics.reservoir" % "hdrhistogram-metrics-reservoir" % "1.1.2"
    val hdrhistogram = "org.hdrhistogram" % "HdrHistogram" % "2.1.9"

    object scala extends SimpleModule {
      override val groupId: String = "nl.grons"
      override val artifactIdRoot: String = "metrics4"
      override def version: String = "4.0.1"

      def all = Seq( core, akka, hdr )
      val core = module( "scala" )
      val akka = module( "akka_a25" )
      val hdr = module( "scala-hdr" )
    }
//    object kamon {
//      val version = "0.6.5"
//      def module( id: String, v: String = version ) = "io.kamon" %% s"kamon-$id" % v
////      def all: Seq[sbt.ModuleID] = Seq.empty[sbt.ModuleID] // Seq( core, scala, akka, /*akkaRemote,*/ system, statsd )
//      def all: Seq[sbt.ModuleID] = Seq( sigarLoader )
//
//      val core = module( "core" )
//      val scala = module( "scala" )
//      val akka = module( "akka-2.4", "0.6.6" )
//      // val akkaRemote = "io.kamon" % "kamon-akka-remote-2.4" % "" // module( "akka-remote", "0.6.6" )
//      val system = module( "system-metrics" )
//      val statsd = module( "statsd" )
//      val logReporter = module( "log-reporter" )
//      val sigarLoader = "io.kamon" % "sigar-loader" % "1.6.6-rev002"
//    }
  }

  val uritemplate = "com.damnhandy" % "handy-uri-templates" % "2.1.7"
  val enumeratum = "com.beachape" %% "enumeratum" % "1.5.13"
  val newtype = "io.estatico" %% "newtype" % "0.4.2"
  val snowflake = "com.softwaremill.common" %% "id-generator" % "1.2.0"
//    val guava = "com.google.guava" % "guava" % "21.0"
  // val offheap = "sh.den" % "scala-offheap_2.11" % "0.1"
  val fastutil = "it.unimi.dsi" % "fastutil" % "8.2.1" withSources() withJavadoc()
  val bloomFilter = "com.github.alexandrnikitin" % "bloom-filter_2.11" % "0.10.1" withSources() withJavadoc()
//  val json4sJackson = "org.json4s" %% "json4s-jackson" % "3.6.1"
//    val uuid = "com.eaio.uuid" % "uuid" % "3.4"
//    val eaioUuid = "com.github.stephenc.eaio-uuid" % "uuid" % "3.4.2"
  val scalaUuid = "io.jvm.uuid" %% "scala-uuid" % "0.2.4"
  val config = "com.typesafe" % "config" % "1.3.3"
  val ficus = "com.iheart" %% "ficus" % "1.4.3"
  val shapeless = "com.chuusai" %% "shapeless" % "2.3.3" withSources() withJavadoc()
  val parboiled = "org.parboiled" %% "parboiled" % "2.1.4"
  val inflector = "org.atteo" % "evo-inflector" % "1.2.2"
  val squants = "org.typelevel"  %% "squants"  % "1.3.0"
  val lang = "org.apache.commons" % "commons-lang3" % "3.5"
  val codec = "commons-codec" % "commons-codec" % "1.11"
  val math3 = "org.apache.commons" % "commons-math3" % "3.6.1" withSources() withJavadoc()
//    val suanshu = "com.numericalmethod" % "suanshu" % "3.4.0" intransitive()  // don't want to use due to $$$
  val scopt = "com.github.scopt" %% "scopt" % "3.5.0"
  val pyrolite = "net.razorvine" % "pyrolite" % "4.19"
  val msgpack = "org.velvia" %% "msgpack4s" % "0.6.0"
  val prettyprint = "com.lihaoyi" %% "pprint" % "0.5.3"

  val hadoopClient = "org.apache.hadoop" % "hadoop-client" % "2.8.0" intransitive // exclude( "log4j", "log4j" )

  object circe extends SimpleModule {
    override def groupId: String = "io.circe"
    override def artifactIdRoot: String = "circe"
    override def version: String = "0.9.3"
    
    def all = Seq ( core, generic, parser )

    val core = module( "core" )
    val generic = module( "generic" )
    val parser = module( "parser" )
  }
  
  object avro extends SimpleModule {
    override val groupId = "org.apache.avro"
    override val artifactIdRoot = "avro"
    override val version = "1.8.1"
    override val isScala = false

    def all = Seq( core, scavro )
    val core = module( "" )
    val tools = module( "tools" )
    val mapred = module( "mapred" )
    val scavro = "org.oedura" %% "scavro" % "1.0.2"
  }

  object betterFiles extends SimpleModule {
    override val groupId = "com.github.pathikrit"
    override val artifactIdRoot = "better-files"
    override val version = "3.6.0"
    val core = module( "" )
    val akka = module( "akka" )
    def all = Seq( core, akka )
  }

  object quality {
    val scalatest = "org.scalatest" %% "scalatest" % "3.0.5" withSources() withJavadoc()
    val scalacheck = "org.scalacheck" %% "scalacheck" % "1.14.0"

    val cats = "com.ironcorelabs" %% "cats-scalatest" % "2.2.0"
    val inmemory = "com.github.dnvriend" %% "akka-persistence-inmemory" % "2.5.1.0"

    object mockito extends SimpleModule {
      override val groupId = "org.mockito"
      override val artifactIdRoot = "mockito"
      override val version = "2.22.0"
      override val isScala = false
      val core = module( "core" ) withSources() withJavadoc()
    }
  }


  val commonTestDependencies: Seq[ModuleID] = Scope.test(
    quality.scalatest,
    quality.mockito.core
  )

  val commonDependencies: Seq[ModuleID] = {
    logging.all ++
    Seq(
      enumeratum,
      config,
      ficus,
      shapeless,
      prettyprint
    ) ++
    commonTestDependencies

  }

  val defaultDependencyOverrides: Seq[sbt.librarymanagement.ModuleID] = Seq.empty[sbt.librarymanagement.ModuleID]
//  val defaultDependencyOverrides: Set[sbt.ModuleID] = Set.empty[sbt.ModuleID]
}
