package com.github.dmrolfs.scalahal.hal

import org.scalatest.{ Matchers, Tag, WordSpec }
import journal._

class CuriesSpec extends WordSpec with Matchers {
  private val log = Logger[CuriesSpec]

  object WIP extends Tag( "wip" )

  "A Curie" should {
    "build registry with curies" in {
      val curies = Curies.fromLinks(
        Links.linkingTo
          .curi( "x", "http://example.com/rels/{rel}" )
          .curi( "y", "http://example.org/rels/{rel}" )
          .build()
      )

      curies.resolve( "http://example.com/rels/foo" ) shouldBe "x:foo"
      curies.resolve( "http://example.org/rels/bar" ) shouldBe "y:bar"
    }

    "expand full rel" in {
      val curies = Curies.fromLinks(
        Links.linkingTo
          .curi( "x", "http://example.com/rels/{rel}" )
          .build()
      )

      curies.expand( "http://example.com/rels/foo" ) shouldBe "http://example.com/rels/foo"
      curies.expand( "item" ) shouldBe "item"
    }

    "fail to register non curi link" in {
      an[IllegalArgumentException] should be thrownBy (
        Curies.empty.register( Link.link( "foo", "http://example.com/foo" ) )
      )
    }

    "resolve full uri" in {
      val registry = Curies.empty.register(
        Link.curi( "o", "http://dmrolfs.github.com/rels/{rel}" )
      )
      registry.resolve( "http://dmrolfs.github.com/rels/foo" ) shouldBe "o:foo"
    }

    "resolve curied uri" in {
      val registry = Curies.empty.register(
        Link.curi( "o", "http://dmrolfs.github.com/rels/{rel}" )
      )

      registry.resolve( "o:foo" ) shouldBe "o:foo"
    }

    "resolve unknown full uri" in {
      val registry = Curies.empty.register(
        Link.curi( "o", "http://dmrolfs.github.com/rels/{rel}" )
      )

      registry.resolve( "http://github.com/some/other" ) shouldBe "http://github.com/some/other"
    }

    "resolve unknown curied uri" in {
      val registry = Curies.empty.register(
        Link.curi( "o", "http://dmrolfs.github.com/rels/{rel}" )
      )

      registry.resolve( "x:other" ) shouldBe "x:other"
      registry.resolve( "http://www.example.com/some/other" ) shouldBe "http://www.example.com/some/other"
    }

    "merge registries" in {
      val registry = Curies.empty.register(
        Link.curi( "x", "http://dmrolfs.github.com/rels/{rel}" )
      )

      val other = Curies.empty.register(
        Link.curi( "u", "http://www.github.com/rels/{rel}" )
      )

      val merged = registry mergeWith other

      merged.resolve( "http://dmrolfs.github.com/rels/foo" ) shouldBe "x:foo"
      merged.resolve( "http://www.github.com/rels/foo" ) shouldBe "u:foo"
    }

    "merge by replacing existing with other" taggedAs WIP in {
      val registry = Curies.empty.register(
        Link.curi( "x", "http://dmrolfs.github.com/rels/{rel}" )
      )

      val other = Curies.empty.register(
        Link.curi( "x", "http://www.github.com/rels/{rel}" )
      )

      val merged = registry mergeWith other

      log.info( s"merged = [${merged}]" )

      pending
      // this seems wrong to pass since existing (first) should be replaced, so should result be full, unknown URI?
      merged.resolve( "http://dmrolfs.github.com/rels/foo" ) shouldBe "x:foo"
      merged.resolve( "http://www.github.com/rels/foo" ) shouldBe "x:foo"
      //  @Test
      //  public void shouldMergeByReplacingExistingWithOther() {
      //    // given
      //    final Curies registry = emptyCuries();
      //    registry.register(curi("x", "http://x.otto.de/rels/{rel}"));
      //    final Curies other = emptyCuries();
      //    other.register(curi("x", "http://spec.otto.de/rels/{rel}"));
      //    // when
      //    final Curies merged = registry.mergeWith(other);
      //    // then
      //    assertThat(merged.resolve("http://spec.otto.de/rels/foo"), is("x:foo"));
      //  }
    }

    //  @Test
    //  public void shouldMergeEmptyRegistryWithNonEmpty() {
    //    // given
    //    final Curies empty = emptyCuries();
    //    final Curies other = emptyCuries();
    //    other.register(curi("o", "http://spec.otto.de/rels/{rel}"));
    //    // when
    //    final Curies merged = empty.mergeWith(other);
    //    // then
    //    assertThat(empty, is(emptyCuries()));
    //    assertThat(merged.resolve("http://spec.otto.de/rels/foo"), is("o:foo"));
    //  }
    //
    //  @Test
    //  public void shouldExpandCuri() {
    //    // given
    //    final Curies curies = Curies.curies(linkingTo().curi("x", "http://example.com/rels/{rel}").build());
    //    // when
    //    final String expanded = curies.expand("x:foo");
    //    // then
    //    assertThat(expanded, is("http://example.com/rels/foo"));
    //  }
    //
    //  @Test
    //  public void shouldReturnCuriIfNotResolvable() {
    //    // given
    //    final Curies curies = emptyCuries();
    //    // when
    //    final String expanded = curies.expand("x:foo");
    //    // then
    //    assertThat(expanded, is("x:foo"));
    //  }
    //
    //  @Test
    //  public void shouldReturnCuriIfAlreadyResolved() {
    //    // given
    //    final Curies curies = Curies.curies(linkingTo().curi("x", "http://example.com/rels/{rel}").build());
    //    // when
    //    final String expanded = curies.expand("http://example.com/rels/foo");
    //    // then
    //    assertThat(expanded, is("http://example.com/rels/foo"));
    //  }
    //}
  }
}
