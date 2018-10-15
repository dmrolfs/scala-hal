package com.github.dmrolfs.scalahal.hal

import org.scalatest.{ Matchers, OptionValues, Tag, WordSpec }
import journal._
import com.github.dmrolfs.scalahal.hal.{ Embedded => E, Link => L, Links => LS }

class EmbeddedSpec extends WordSpec with Matchers with OptionValues {
  private val log = Logger[EmbeddedSpec]

  object WIP extends Tag( "wip" )

  "Embedded" should {
    "create empty embedded" in {
      val e = E.empty
      e.itemsBy( "foo" ) shouldBe empty
    }

    "create embedded" in {
      val e = E.embedded( "foo", Seq( HalRepresentation() ) )
      e.itemsBy( "foo" ) should have size 1
    }

    "create single embedded" in {
      val e = E.embedded( "foo", HalRepresentation() )
      e.itemsBy( "foo" ) should have size 1
    }

    "create embedded as array" in {
      val e = E.embedded( "foo", Seq( HalRepresentation() ) )
      e.hasItem( "foo" ) shouldBe true
      e.itemsBy( "foo" ) should have size 1
    }

    "create embedded as single object" in {
      val e = E.embedded( "foo", HalRepresentation() )
      e.hasItem( "foo" ) shouldBe true
      e.isArray( "foo" ) shouldBe false
    }

    "create embedded with builder" in {
      val e = E
        .embeddedBuilder()
        .withRepresentations( "foo", Seq( HalRepresentation() ) )
        .withRepresentations( "bar", Seq( HalRepresentation() ) )
        .build()
      e.itemsBy( "foo" ) should have size 1
      e.itemsBy( "bar" ) should have size 1
      e.itemsBy( "foobar" ) shouldBe empty
    }

    "add rel to embedded using builder" in {
      val e = E
        .fromPrototype( E.embedded( "foo", Seq( HalRepresentation() ) ) )
        .withRepresentations( "bar", Seq( HalRepresentation() ) )
        .build()

      e.itemsBy( "foo" ) should have size 1
      e.itemsBy( "bar" ) should have size 1
      e.itemsBy( "foobar" ) shouldBe empty
    }

    "get items as type" in {
      val e = E.embedded(
        "foo",
        Seq(
          HalRepresentation(),
          HalRepresentation()
        )
      )

      val reps = e.itemsBy( "foo" )
      reps should have size 2
    }

    "get all link relations" in {
      val e = E
        .embeddedBuilder()
        .withRepresentation( "foo", HalRepresentation() )
        .withRepresentation( "bar", HalRepresentation() )
        .build()

      e.rels should contain allOf ("foo", "bar")
    }

    "replace rels with curied rels" taggedAs WIP in {
      val curies = Curies(
        Seq( L.curi( "test", "http://example.com/rels/{rel}" ) )
      )

      val e = E
        .embeddedBuilder()
        .withRepresentation( "http://example.com/rels/foo", HalRepresentation() )
        .withRepresentation( "http://example.com/rels/bar", HalRepresentation() )
        .build()

      log.info( s"embedded = $e" )
      log.info( s"embedded.using(curies) = ${e.using( curies )}" )
      log.info( s"embedded.using(curies).rels = ${e.using( curies ).rels}" )

      e.using( curies ).rels should contain allOf ("test:foo", "test:bar")
    }

    "replace nested rels with curied rels" in {
      val curies = Curies( Seq( L.curi( "test", "http://example.com/rels/{rel}" ) ) )

      val e = E
        .embeddedBuilder()
        .withRepresentations(
          "http://example.com/rels/foo",
          Seq(
            HalRepresentation(
              embedded = E
                .embeddedBuilder()
                .withRepresentation(
                  "http://example.com/rels/bar",
                  HalRepresentation()
                )
                .build()
            )
          )
        )
        .using( curies )
        .build()

      e.rels should contain( "test:foo" )
      e.itemsBy( "test:foo" )( 0 ).embedded.rels should contain( "test:bar" )
    }

    "replace nested link rels with curied link rels" in {
      val curies = Curies( Seq( L.curi( "test", "http://example.com/rels/{rel}" ) ) )
      val e = E
        .embeddedBuilder()
        .withRepresentation(
          "http://example.com/rels/foo",
          HalRepresentation(
            LS.linkingTo
              .single( L.link( "http://example.com/rels/bar", "http://example.com" ) )
              .build()
          )
        )
        .using( curies )
        .build()

      e.itemsBy( "test:foo" )( 0 ).links.rels should contain( "test:bar" )
    }

    "replace rels with curied rels using builder" in {
      val curies = Curies(
        Seq( L.curi( "test", "http://example.com/rels/{rel}" ) )
      )

      val e = E
        .embeddedBuilder()
        .withRepresentation( "http://example.com/rels/foo", HalRepresentation() )
        .withRepresentation( "http://example.com/rels/bar", HalRepresentation() )
        .using( curies )
        .build()

      e.rels should contain allOf ("test:foo", "test:bar")
    }
  }
}
