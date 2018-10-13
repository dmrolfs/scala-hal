package com.github.dmrolfs.scalahal.hal

import org.scalatest.{ Matchers, OptionValues, Tag, WordSpec }
import com.github.dmrolfs.scalahal.hal.{ Link => L, Links => LS }
import journal._

class LinksBuilderSpec extends WordSpec with Matchers with OptionValues {
  private val log = Logger[LinksBuilderSpec]

  object WIP extends Tag( "wip" )

  "LinksBuilder" should {
    "add links with new rel" in {
      val links = LS.linkingTo.self( "/foo" ).build()
      val extended = LS
        .fromPrototype( links )
        .withLinks( LS.linkingTo.item( "/bar" ).build() )
        .build()

      extended.linkBy( "self" ) should be( 'defined )
      extended.linkBy( "item" ) should be( 'defined )
    }
  }

  "add link with new rel" in {
    val links = LS.linkingTo.self( "/foo" ).build()
    val extended = LS
      .fromPrototype( links )
      .array( L.item( "/bar" ) )
      .build()

    extended.linkBy( "self" ) should be( 'defined )
    extended.linkBy( "item" ) should be( 'defined )
  }

  "add link to existing item rel" in {
    val links = LS.linkingTo.item( "/foo" ).build()
    val extended = LS
      .fromPrototype( links )
      .array( L.item( "/bar" ) )
      .build()

    extended.linksBy( "item" ) should have size 2
  }

  "add link to existing array rel" in {
    val links = LS.linkingTo.array( L.link( "some-rel", "/foo" ) ).build()
    val extended = LS
      .fromPrototype( links )
      .array( L.link( "some-rel", "/bar" ) )
      .build()

    extended.linksBy( "some-rel" ) should have size 2
  }

  "not add link to existing array rel using single" in {
    val links = LS.linkingTo.array( L.link( "some-rel", "/foo" ) ).build()

    val caught = the[IllegalStateException] thrownBy {
      LS.fromPrototype( links ).single( L.link( "some-rel", "/bar" ) ).build()
    }

    caught.getMessage should fullyMatch regex """The Link-Relation Type '.*' of the Link is already present.""".r
  }

  "fail to add link to existing single rel" in {
    val links = LS.linkingTo.single( L.link( "some-rel", "/foo" ) ).build()

    val caught = the[IllegalStateException] thrownBy {
      LS.fromPrototype( links ).array( L.link( "some-rel", "/bar" ) ).build()
    }

    caught.getMessage should fullyMatch regex {
      ("""Unable to add links with rel=\[.*\] as there is already a single Link Object added """ +
      "for this link-relation type").r
    }
  }

  "add links to existing array rel" in {
    val links = LS.linkingTo.array( L.link( "some-rel", "/foo" ) ).build()
    val extended = LS
      .fromPrototype( links )
      .array( L.item( "/bar" ), L.item( "/foobar" ) )
      .build()

    extended.linksBy( "item" ) should have size 2
  }

  "not duplicate link" in {
    val links = LS.linkingTo.item( "/foo" ).item( "/bar" ).build()
    val extended = LS
      .fromPrototype( links )
      .array( L.item( "/bar" ) )
      .build()

    extended.linksBy( "item" ) should have size 2
  }

  "add different but not equivalent links" in {
    val links = LS.linkingTo
      .array(
        L.linkBuilder( "myrel", "/foo" )
          .withType( "some type" )
          .withProfile( "some profile" )
          .build()
      )
      .build()

    val extended = LS
      .fromPrototype( links )
      .array(
        L.linkBuilder( "myrel", "/foo" )
          .withType( "some DIFFERENT type" )
          .withProfile( "some profile" )
          .withTitle( "ignored title" )
          .withDeprecation( "ignored deprecation" )
          .withHrefLang( "ignored language" )
          .withName( "ignored name" )
          .build()
      )
      .build()

    extended.linksBy( "myrel" ) should have size 2
  }

  "not add equivalent links" in {
    val links = LS.linkingTo
      .array(
        L.linkBuilder( "myrel", "/foo" )
          .withType( "some type" )
          .withProfile( "some profile" )
          .build()
      )
      .build()

    val extended = LS
      .fromPrototype( links )
      .array(
        L.linkBuilder( "myrel", "/foo" )
          .withType( "some type" )
          .withProfile( "some profile" )
          .withName( "foo" )
          .build()
      )
      .build()

    extended.linksBy( "myrel" ) should have size 1
  }

  "merge curies" in {
    val someLinks = LS.linkingTo.curi( "x", "http://example.com/rels/{rel}" )
    val otherLinks = LS.linkingTo.curi( "y", "http://example.org/rels/{rel}" ).build()
    val mergedLinks = someLinks.withLinks( otherLinks ).build()

    mergedLinks.linksBy( "curies" ) should contain allOf (
      L.curi( "x", "http://example.com/rels/{rel}" ),
      L.curi( "y", "http://example.org/rels/{rel}" )
    )
  }
}
