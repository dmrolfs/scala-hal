package com.github.dmrolfs.scalahal.hal

import org.scalatest.{ Matchers, OptionValues, Tag, WordSpec }
import com.github.dmrolfs.scalahal.hal.{ Link => L, LinkPredicate => LP, Links => LS }
import journal._

class LinksSpec extends WordSpec with Matchers with OptionValues {
  private val log = Logger[LinksSpec]

  object WIP extends Tag( "wip" )

  "Links" should {
    "create empty Links" in {
      val links = Links.empty
      links shouldBe empty
    }

    "create Links" in {
      val links = LS.linkingTo.self( "http://example.org" ).build()
      val self = links.linkBy( "self" )
      self should be( 'defined )
      self.value.rel shouldBe "self"
      self.value.href shouldBe "http://example.org"
    }

    "create multiple links" in {
      val links = LS.linkingTo
        .self( "http://example.org/items/42" )
        .single( L.collection( "http://example.org/items" ) )
        .build()

      links.linkBy( "self" ) should be( 'defined )
      links.linkBy( "collection" ) should be( 'defined )
    }

    "create multiple links from list" in {
      val links = LS.linkingTo
        .single( Seq( L.self( "http://example.org/items" ) ) )
        .array(
          Seq(
            L.item( "http://example.org/items/1" ),
            L.item( "http://example.org/items/2" )
          )
        )
        .build()

      links.linkBy( "self" ) should be( 'defined )
      links.linkBy( "item" ) should be( 'defined )
      links.linksBy( "item" ) should have size 2
    }

    "create multiple links using builder" in {
      val links = LS.linkingTo
        .self( "http://example.org/items" )
        .array(
          L.item( "http://example.org/items/1" ),
          L.item( "http://example.org/items/2" )
        )
        .build()

      links.linkBy( "self" ) should be( 'defined )
      links.linkBy( "item" ) should be( 'defined )
      links.linksBy( "item" ) should have size 2
    }

    "get first link" in {
      val links = LS.linkingTo
        .item( "http://example.org/items/42" )
        .item( "http://example.org/items/44" )
        .build()

      links.linkBy( "item" ) should be( 'defined )
      links.linkBy( "item" ).value.href shouldBe "http://example.org/items/42"
    }

    "get first link matching type and profile" in {
      val links = LS.linkingTo
        .array(
          L.linkBuilder( "item", "http://example.org/items/42" )
            .build(),
          L.linkBuilder( "item", "http://example.org/items/42" )
            .withType( "text/plain" )
            .withProfile( "myprofile" )
            .build(),
          L.linkBuilder( "item", "http://example.org/items/42" )
            .withType( "text/html" )
            .withProfile( "THEprofile" )
            .build(),
          L.linkBuilder( "item", "http://example.org/items/42" )
            .withType( "THEtype" )
            .withProfile( "THEprofile" )
            .build()
        )
        .build()

      links.linkBy(
        "item",
        LP.havingType( "THEtype" ).and( LP.havingProfile( "THEprofile" ) )
      ) should be( 'defined )

      links
        .linkBy(
          "item",
          LP.havingType( "THEtype" ).and( LP.havingProfile( "THEprofile" ) )
        )
        .value
        .href shouldBe "http://example.org/items/42"

      links
        .linkBy(
          "item",
          LP.havingType( "text/plain" )
        )
        .value
        .profile
        .value shouldBe "myprofile"

      links
        .linkBy(
          "item",
          LP.havingProfile( "THEprofile" )
        )
        .value
        .`type`
        .value shouldBe "text/html"
    }

    "get no link for unknown rel" in {
      val links = LS.empty
      links.linkBy( "item" ) should not be ('defined)
    }

    "get no link for filtered unknown rel" in {
      val links = LS.empty
      links.linkBy( "item", LP.havingType( "text/plain" ) ) should not be ('defined)
    }

    "get no link for filtered link" in {
      val links = LS.linkingTo
        .item( "http://example.org/items/42" )
        .build()

      links.linkBy( "item", LP.havingType( "text/plain" ) ) should not be ('defined)
    }

    "get all links" in {
      val links = LS.linkingTo
        .item( "http://example.org/items/42" )
        .item( "http://example.org/items/44" )
        .build()

      links.linksBy( "item" ) should have size 2
      links.linksBy( "item" )( 0 ).href shouldBe "http://example.org/items/42"
      links.linksBy( "item" )( 1 ).href shouldBe "http://example.org/items/44"
    }

    "remove links" in {
      val l1 = LS.linkingTo
        .item( "http://example.org/items/42" )
        .item( "http://example.org/items/44" )
        .build()

      val l2 = l1.remove( "item" )
      val l3 = l2.remove( "does-not-exist" )
      l3 shouldBe empty
    }

    "get links matching name" in {
      val links = LS.linkingTo
        .array(
          L.linkBuilder( "item", "http://example.org/items/42" )
            .withName( "Foo" )
            .withType( "text/html" )
            .build(),
          L.linkBuilder( "item", "http://example.org/items/42" ).withName( "Foo" ).build(),
          L.linkBuilder( "item", "http://example.org/items/42" ).withName( "Bar" ).build()
        )
        .build()

      links.linksBy( "item", LP.havingName( "Foo" ) ) should contain allOf (
        L.linkBuilder( "item", "http://example.org/items/42" )
          .withName( "Foo" )
          .withType( "text/html" )
          .build(),
        L.linkBuilder( "item", "http://example.org/items/42" )
          .withName( "Foo" )
          .build()
      )

      links.linksBy(
        "item",
        LP.havingName( "Foo" ).and( LP.havingType( "text/html" ) )
      ) should contain(
        L.linkBuilder( "item", "http://example.org/items/42" )
          .withName( "Foo" )
          .withType( "text/html" )
          .build()
      )
    }

    "get links matching name or empty" in {
      val links = LS.linkingTo
        .array(
          L.linkBuilder( "item", "http://example.org/items/41" ).build(),
          L.linkBuilder( "item", "http://example.org/items/42" ).withName( "Foo" ).build(),
          L.linkBuilder( "item", "http://example.org/items/43" ).withName( "Bar" ).build()
        )
        .build()

      links.linksBy( "item", LP.optionallyHavingName( "Foo" ) ) should contain allOf (
        L.linkBuilder( "item", "http://example.org/items/41" ).build(),
        L.linkBuilder( "item", "http://example.org/items/42" ).withName( "Foo" ).build()
      )
    }

    "get all links matching type and profile" in {
      val links = LS.linkingTo
        .array(
          L.linkBuilder( "item", "http://example.org/items/42" ).build(),
          L.linkBuilder( "item", "http://example.org/items/42" )
            .withType( "text/plain" )
            .withProfile( "myprofile" )
            .build(),
          L.linkBuilder( "item", "http://example.org/items/42" )
            .withType( "text/html" )
            .withProfile( "THEprofile" )
            .build(),
          L.linkBuilder( "item", "http://example.org/items/42" )
            .withType( "THEtype" )
            .withProfile( "THEprofile" )
            .build()
        )
        .build()

      links
        .linksBy(
          "item",
          LP.havingType( "THEtype" )
            .and( LP.havingProfile( "THEprofile" ) )
        )( 0 )
        .href shouldBe "http://example.org/items/42"

      links
        .linksBy(
          "item",
          LP.havingType( "text/plain" )
        )( 0 )
        .profile
        .value shouldBe "myprofile"

      links
        .linksBy(
          "item",
          LP.havingProfile( "THEprofile" )
        )( 0 )
        .`type`
        .value shouldBe "text/html"
    }

    "get all link relations" in {
      val links = LS.linkingTo
        .array(
          L.link( "foo", "http://example.org/foo" ),
          L.link( "bar", "http://example.org/bar" )
        )
        .build()

      links.rels should contain allOf ("foo", "bar")
    }

    "get empty list for unknown rel" in {
      val links = LS.empty
      links.linksBy( "item" ) shouldBe empty
    }

    "get curied links from full rel" taggedAs WIP in {
      val links = LS.linkingTo
        .curi( "o", "http://dmrolfs.github.com/rels/{rel}" )
        .array(
          L.link( "o:product", "http://example.org/products/42" ),
          L.link( "o:product", "http://example.org/products/44" )
        )
        .build()

      val productHrefs = links
        .linksBy( "http://dmrolfs.github.com/rels/product" )
        .map { _.href }

      productHrefs should contain allOf (
        "http://example.org/products/42",
        "http://example.org/products/44"
      )
    }

    "get curied links from full rel with predicate" in {
      val links = LS.linkingTo
        .curi( "o", "http://spec.otto.de/rels/{rel}" )
        .array(
          L.linkBuilder( "o:product", "http://example.org/products/42" )
            .withName( "First" )
            .build(),
          L.linkBuilder( "o:product", "http://example.org/products/44" )
            .withName( "Second" )
            .build()
        )
        .build()

      val productHrefs = links
        .linksBy(
          "http://spec.otto.de/rels/product",
          LP.havingName( "Second" )
        )
        .map { _.href }

      productHrefs should contain( "http://example.org/products/44" )
    }

    "get curied links from curied rel" in {
      val links = LS.linkingTo
        .curi( "o", "http://spec.otto.de/rels/{rel}" )
        .array( L.link( "o:product", "http://example.org/products/42" ) )
        .array( L.link( "o:product", "http://example.org/products/44" ) )
        .build()

      val productHrefs = links.linksBy( "o:product" ) map { _.href }

      productHrefs should contain allOf (
        "http://example.org/products/42",
        "http://example.org/products/44"
      )
    }

    "get curied links from Curies rel with predicate" in {
      val links = LS.linkingTo
        .curi( "o", "http://spec.otto.de/rels/{rel}" )
        .array(
          L.linkBuilder( "o:product", "http://example.org/products/42" )
            .withName( "First" )
            .build(),
          L.linkBuilder( "o:product", "http://example.org/products/44" )
            .withName( "Second" )
            .build()
        )
        .build()

      val productHrefs = links
        .linksBy( "o:product", LP.havingName( "Second" ) )
        .map { _.href }

      productHrefs should contain( "http://example.org/products/44" )
    }

    "replace full rels with curied rels" in {
      val links = LS.linkingTo
        .curi( "o", "http://spec.otto.de/rels/{rel}" )
        .array(
          L.link( "http://spec.otto.de/rels/product", "http://example.org/products/42" ),
          L.link( "http://spec.otto.de/rels/product", "http://example.org/products/44" )
        )
        .build()

      val productHrefs = links.linksBy( "o:product" ) map { _.href }

      productHrefs should contain allOf (
        "http://example.org/products/42",
        "http://example.org/products/44"
      )
    }

    "replace full rels with curied rels after construction" in {
      val links = LS.linkingTo
        .array(
          L.link( "http://spec.otto.de/rels/product", "http://example.org/products/42" ),
          L.link( "http://spec.otto.de/rels/product", "http://example.org/products/44" )
        )
        .curi( "o", "http://spec.otto.de/rels/{rel}" )
        .build()

      val productHrefs = links.linksBy( "o:product" ) map { _.href }

      productHrefs should contain allOf (
        "http://example.org/products/42",
        "http://example.org/products/44"
      )
    }

    "ignore missing curies" in {
      val links = LS.linkingTo
        .array(
          L.link( "o:product", "http://example.org/products/42" ),
          L.link( "o:product", "http://example.org/products/44" )
        )
        .build()

      val productHrefs = links.linksBy( "o:product" ) map { _.href }

      productHrefs should contain allOf (
        "http://example.org/products/42",
        "http://example.org/products/44"
      )
    }
  }
}
