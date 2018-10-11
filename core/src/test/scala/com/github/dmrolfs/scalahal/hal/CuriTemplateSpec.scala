package com.github.dmrolfs.scalahal.hal

import org.scalatest.{ Matchers, OptionValues, Tag, WordSpec }
import journal._

class CuriTemplateSpec extends WordSpec with Matchers with OptionValues {
  private val log = Logger[CuriTemplateSpec]

  private val someCuri = Link.curi( "x", "http://example.org/rels/{rel}" )
  private val someOtherCuri = Link.curi( "y", "http://example.com/link-relations/{rel}" )
  private val curies = Seq( someCuri, someOtherCuri )

  private val someRel = "http://example.org/rels/product"
  private val someCuriedRel = "x:product"

  private val nonMatchingRel = "http://example.org/link-relations/product"
  private val nonMatchingCuriedRel = "z:product"

  object WIP extends Tag( "wip" )

  "A CuriTemplate" when {
    "failed" should {
      "not create curi template for wrong rel" in {
        val caught = the[IllegalArgumentException] thrownBy CuriTemplate.curiTemplateFor(
          //        Link.link( "foo", "/bar" )
          Link.linkBuilder( "curies", "/bar" ).withName( "x" ).build()
        )

        caught.getMessage should endWith( "CURI does not contain the required {rel} placeholder." )
      }

      "not create curi template for wrong href" in {
        val caught = the[IllegalArgumentException] thrownBy CuriTemplate.curiTemplateFor(
          Link.linkBuilder( "curies", "/bar" ).withName( "x" ).build()
        )
        caught.getMessage should endWith regex "required.*placeholder.*".r
      }

      "not create curi template with missing name" in {
        val caught = the[IllegalArgumentException] thrownBy CuriTemplate.curiTemplateFor(
          Link.link( "curies", "/bar/{rel}" )
        )

        caught.getMessage should startWith( "requirement failed: Parameter is not a CURI link." )
      }
    }

    "matching" should {
      "find matching UriTemplate for expanded rel" in {
        val template = CuriTemplate.matchingCuriTemplateFor( curies, someRel )
        template should be( 'defined )
        template.value.curi shouldBe someCuri
      }

      "find matching UriTemplate for curied rel" taggedAs WIP in {
        val template = CuriTemplate.matchingCuriTemplateFor( curies, someCuriedRel )
        template should be( 'defined )
        template.value.curi shouldBe someCuri
      }

      "not find matching UriTemplate" in {
        val template = CuriTemplate.matchingCuriTemplateFor( curies, nonMatchingRel )
        template shouldNot be( 'defined )
      }

      "match" in {
        val template = CuriTemplate curiTemplateFor someCuri
        Seq( someRel, someCuriedRel ) foreach { e =>
          template.isMatching( e ) shouldBe true
        }
        template.isMatchingExpandedRel( someRel ) shouldBe true
        template.isMatchingCuriedRel( someCuriedRel ) shouldBe true
      }

      "not match" in {
        val template = CuriTemplate curiTemplateFor someCuri
        Seq( nonMatchingRel, nonMatchingCuriedRel ) foreach { e =>
          template.isMatching( e ) shouldNot be( true )
        }
        template.isMatchingExpandedRel( someCuriedRel ) shouldNot be( true )
        template.isMatchingCuriedRel( someRel ) shouldNot be( true )
      }
    }

    "extracting" should {
      "curied rel" in {
        val template = CuriTemplate.curiTemplateFor( someCuri )
        template.curiedRelFrom( someRel ) shouldBe "x:product"
        template.curiedRelFrom( someCuriedRel ) shouldBe "x:product"
      }

      "fail to extract curied rel for non matching rel" in {
        val caught = the[IllegalArgumentException] thrownBy {
          CuriTemplate
            .curiTemplateFor( someCuri )
            .curiedRelFrom( nonMatchingRel )
        }
        caught.getMessage should fullyMatch regex """Rel \[.*\] does not match the CURI template.""".r
      }

      "expand rel" in {
        val template = CuriTemplate.curiTemplateFor( someCuri )
        template.expandedRelFrom( someRel ) shouldBe someRel
        template.expandedRelFrom( someCuriedRel ) shouldBe someRel
      }

      "fail to expand non matching rel" in {
        val caught = the[IllegalArgumentException] thrownBy {
          CuriTemplate.curiTemplateFor( someCuri ).expandedRelFrom( nonMatchingRel )
        }

        caught.getMessage should fullyMatch regex """Rel \[.*\] does not match the CURI template.""".r
      }

      "extract placeholder value" in {
        val template = CuriTemplate.curiTemplateFor( someCuri )
        template.relPlaceHolderFrom( someRel ) shouldBe "product"
        template.relPlaceHolderFrom( someCuriedRel ) shouldBe "product"
      }

      "fail to extract placeholder rel for non matching rel" in {
        val caught = the[IllegalArgumentException] thrownBy {
          CuriTemplate.curiTemplateFor( someCuri ).relPlaceHolderFrom( nonMatchingRel )
        }

        caught.getMessage should fullyMatch regex """Rel \[.*\] does not match the CURI template.""".r
      }
    }
  }
}
