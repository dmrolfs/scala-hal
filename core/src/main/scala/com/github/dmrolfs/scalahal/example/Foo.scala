package com.github.dmrolfs.scalahal.example
import com.github.dmrolfs.scalahal.HalEncoder
import com.github.dmrolfs.scalahal.hal.{ HalRepresentation, Link, Links }

case class Foo( barIds: Seq[Long], age: Int )

object Foo {
  implicit val encoder = HalEncoder.pure[Foo] { foo =>
    HalRepresentation()
      .add(
        Links.linkingTo
          .self( "http://example.com/test/foo" )
          .curi( "bars", "http://example.com/bars/{rel}" )
          .single( Link.link( "bars:1", "http://example.org/bars/1" ) )
          .build()
      )
      .addAttribute( "age", foo.age )
  }
}
