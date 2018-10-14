package com.github.dmrolfs.scalahal.hal

import org.scalatest.{ Matchers, WordSpec }
import com.github.dmrolfs.scalahal.hal.{ Link => L, LinkPredicate => LP }
import journal._

class LinkPredicateSpec extends WordSpec with Matchers {
  private val log = Logger[LinkPredicateSpec]

  "LinkPredicates" when {
    "HavingType" should {
      "match type" in {
//        val links = LS.linkingTo
//          .array(
//            L.linkBuilder( "item", "http://example.org/items/42" )
//              .build(),
//            L.linkBuilder( "item", "http://example.org/items/42" )
//              .withType( "text/plain" )
//              .withProfile( "myprofile" )
//              .build(),
//            L.linkBuilder( "item", "http://example.org/items/42" )
//              .withType( "text/html" )
//              .withProfile( "THEprofile" )
//              .build(),
//            L.linkBuilder( "item", "http://example.org/items/42" )
//              .withType( "THEtype" )
//              .withProfile( "THEprofile" )
//              .build()
//          )
//          .build()

//        val link = L
//          .linkBuilder( "item", "http://example.org/items/42" )
//          .withType( "text/plain" )
//          .withProfile( "myprofile" )
//          .build()

        val link = L
          .linkBuilder( "item", "http://example.org/items/42" )
          .withType( "THEtype" )
          .withProfile( "THEprofile" )
          .build()

        log.info( s"link = [${link}]" )

        LP.havingType( "THEtype" )( link ) shouldBe true
      }
    }
  }
}
