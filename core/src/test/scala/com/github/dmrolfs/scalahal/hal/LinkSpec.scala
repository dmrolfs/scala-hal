package com.github.dmrolfs.scalahal.hal

import org.scalatest.{ Matchers, OptionValues, WordSpec }
import com.github.dmrolfs.scalahal.hal.{ Link => L }

class LinkSpec extends WordSpec with Matchers with OptionValues {
  "A Link" should {
    "build self link" in {
      val self = L.self( "http://example.org" )
      self.rel shouldBe "self"
      self.href shouldBe "http://example.org"
    }
  }

  "build profile link" in {
    val profile = L.profile( "http://example.org/profiles/test" )
    profile.rel shouldBe "profile"
    profile.href shouldBe "http://example.org/profiles/test"
  }

  "build item link" in {
    val self = L.item( "http://example.org/items/42" )
    self.rel shouldBe "item"
    self.href shouldBe "http://example.org/items/42"
  }

  "build collection link" in {
    val self = L.collection( "http://example.org/items" )
    self.rel shouldBe "collection"
    self.href shouldBe "http://example.org/items"
  }

  "build temnplated link" in {
    val link = L.link( "myRel", "/test/{foo}" )
    link.rel shouldBe "myRel"
    link.href shouldBe "/test/{foo}"
    link.templated shouldBe true
    link.hrefAsTemplate.set( "foo", 42 ).expand() shouldBe "/test/42"
  }

  "build templated links using LinkBuilder" in {
    val self = L
      .linkBuilder( "myRel", "/test/{foo}" )
      .withHrefLang( "de_DE" )
      .withTitle( "title" )
      .withName( "name" )
      .withProfile( "my-profile" )
      .withType( "type" )
      .build()

    self.rel shouldBe "myRel"
    self.href shouldBe "/test/{foo}"
    self.templated shouldBe true
    self.hreflang.value shouldBe "de_DE"
    self.title.value shouldBe "title"
    self.name.value shouldBe "name"
    self.`type`.value shouldBe "type"
    self.profile.value shouldBe "my-profile"
    self.deprecation should not be ('defined)
  }

  "build link using builder" in {
    val link = L
      .linkBuilder( "myRel", "/test/foo" )
      .withHrefLang( "de_DE" )
      .withTitle( "title" )
      .withName( "name" )
      .withProfile( "my-profile" )
      .withType( "type" )
      .build()

    link.rel shouldBe "myRel"
    link.href shouldBe "/test/foo"
    link.templated shouldBe false
    link.hreflang.value shouldBe "de_DE"
    link.title.value shouldBe "title"
    link.name.value shouldBe "name"
    link.`type`.value shouldBe "type"
    link.profile.value shouldBe "my-profile"
  }

  "build deprecated link" in {
    val link = L
      .linkBuilder( "myRel", "/test/foo" )
      .withDeprecation( "http://example.com/whyThisIsDeprecated.html" )
      .build()

    link.deprecation.value shouldBe "http://example.com/whyThisIsDeprecated.html"
  }

  "have properties equals and hashCode" in {
    val link1 = L
      .linkBuilder( "myRel", "/test/foo" )
      .withHrefLang( "de_DE" )
      .withTitle( "title" )
      .withName( "name" )
      .withProfile( "my-profile" )
      .withType( "type" )
      .build()

    val link2 = L
      .linkBuilder( "myRel", "/test/foo" )
      .withHrefLang( "de_DE" )
      .withTitle( "title" )
      .withName( "name" )
      .withProfile( "my-profile" )
      .withType( "type" )
      .build()

    link1 shouldBe link2
    link2 shouldBe link1
    link1.## shouldBe link2.##
  }

  "build curi" in {
    val link = L.curi( "t", "http://example.org/{rel}" )
    link.name.value shouldBe "t"
    link.rel shouldBe "curies"
    link.templated shouldBe true
  }

  "fail to build curi without rel placeholder" in {
    an[IllegalArgumentException] should be thrownBy {
      L.curi( "t", "http://example.org/rel" )
    }
  }

  "be equivalent" in {
    val first = L
      .linkBuilder( "myrel", "/foo" )
      .withType( "some type" )
      .withProfile( "some profile" )
      .build()

    val other = L
      .linkBuilder( "myrel", "/foo" )
      .withType( "some type" )
      .withProfile( "some profile" )
      .withTitle( "ignored title" )
      .withDeprecation( "ignored deprecation" )
      .withHrefLang( "ignored language" )
      .withName( "ignored name" )
      .build()

    first.isEquivalentTo( other ) shouldBe true
  }

  "not be equivalent if href is different" in {
    val first = L.link( "myrel", "/foo" )
    val other = L.link( "myrel", "/bar" )
    first.isEquivalentTo( other ) shouldBe false
  }

  "not be equivalent if rel is different" in {
    val first = L.link( "myrel", "/foo" )
    val other = L.link( "myOtherRel", "/foo" )
    first.isEquivalentTo( other ) shouldBe false
  }

  "not be equivalent if type is different" in {
    val first = L
      .linkBuilder( "myrel", "/foo" )
      .withType( "some type" )
      .build()

    val other = L
      .linkBuilder( "myrel", "/foo" )
      .withType( "some other type" )
      .build()

    first.isEquivalentTo( other ) shouldBe false
  }

  "not be equivalent if profile is different" in {
    val first = L
      .linkBuilder( "myrel", "/foo" )
      .withType( "some profile" )
      .build()

    val other = L
      .linkBuilder( "myrel", "/foo" )
      .withType( "some other profile" )
      .build()

    first.isEquivalentTo( other ) shouldBe false
  }
}
