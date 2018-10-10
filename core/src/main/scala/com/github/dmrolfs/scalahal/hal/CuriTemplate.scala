package com.github.dmrolfs.scalahal.hal

/**
  * <p>
  *   A utility class used to handle user-defined link-relation types and CURI templates.
  * </p>
  * <p>
  *   A CURI is a {@link Link#isTemplated() templated Link} with
  *   {@link Link#getRel() link-relation type } {@code curies}, a {@link Link#name name} and a
  *   {@link Link#getHref() href} containing a single placeholder {@code {rel}}:
  * </p>
  * <pre><code>
  *   {
  *       "_links": {
  *           "curies: [
  *                {"name": "x", "href": "http://example.com/rels/{rel}", "templated": true}
  *           ]
  *       }
  *   }
  * </code></pre>
  * <p>
  *   or:
  * </p>
  * <pre><code>
  *   Link curi = Link.curi("x", "http://example.com/rels/{rel}");
  * </code></pre>
  * <p>
  *   You can construct a {@code CuriTemplate} from a curi like this:
  * </p>
  * <pre><code>
  *   CuriTemplate template = CuriTemplate.curiTemplateFor(curi);
  * </code></pre>
  * <p>
  *   A Link-relation type (rel) specifies the semantics of a {@link Link}. A {@code next} link,
  *   for example, points to the next resource (page) in a sequence of paged resources. While rels
  *   like {@code self}, {@code next} or {@code prev} are predefined types, you may define
  *   user-defined link-relation types as well. If you need to use a user-defined rel like, for
  *   example a rel used to refer to a user resource, you should use URIs like
  *   {@code http://example.org/rels/product}.
  * </p>
  * <p>
  *   Because user-defined link-relation types (or custom rels) are quite verbose, CURIES can be used to abbreviate
  *   them. The following example shows how this will look like in HAL+JSON:
  * </p>
  * <pre><code>
  *   {
  *       "_links": {
  *           "curies: [
  *                {"name": "x", "href": "http://example.com/rels/{rel}", "templated": true}
  *           ],
  *           "x:product": [
  *                {"href": "http://example.com/products/42"},
  *                {"href": "http://example.com/products/43"},
  *                {"href": "http://example.com/products/44"},
  *                {"href": "http://example.com/products/45"}
  *           ],
  *           "x:shoppingCart": {"href": "http://example.com/shoppingCart/4711"}
  *       }
  *   }
  * </code></pre>
  * <p>
  *   Sometimes it is necessary to construct CURIED rels from a expanded rel, or vice versa. The
  *   {@code CuriTemplate} can be used to do things like this.
  * </p>
  *
  * @since 0.1.0
  */
case class CuriTemplate private (
  relPrefix: String,
  curiedRelPrefix: String,
  relSuffix: String,
  curi: Link
) {

  /**
    * Returns true, if the given link-relation type is matching the CuriTemplate pattern, false if
    * not.
    *
    * <p>
    *   Example:
    * </p>
    * <pre><code>
    *   Link curi = Link.curi("x", "http://example.org/rels/{rel}");
    *   CuriTemplate template = CuriTemplate.curiTemplateFor(curi);
    *
    *   // Returns true:
    *   template.isMatching("http://example.org/rels/product");
    *   template.isMatching("x:product"):
    *
    *   // Returns false:
    *   template.isMatching("http://example.com/link-relations/product");
    *   template.isMatching("y:product");
    * </code></pre>
    *
    * @param rel a link-relation type.
    * @return boolean
    * @since 0.1.0
    */
  def isMatching( rel: String ): Boolean = {
    isMatchingExpandedRel( rel ) || isMatchingCuriedRel( rel )
  }

  /**
    * Returns true, if the given link-relation type is a CURI rel matching the CuriTemplate pattern,
    * false if not.
    *
    * <p>
    *   Example:
    * </p>
    * <pre><code>
    *   Link curi = Link.curi("x", "http://example.org/rels/{rel}");
    *   CuriTemplate template = CuriTemplate.curiTemplateFor(curi);
    *
    *   // Returns true:
    *   template.isMatchingCuriedRel("x:product"):
    *
    *   // Returns false:
    *   template.isMatchingCuriedRel("http://example.org/rels/product");
    *   template.isMatchingCuriedRel("http://example.com/link-relations/product");
    *   template.isMatchingCuriedRel("y:product");
    * </code></pre>
    *
    * @param rel a link-relation type.
    * @return boolean
    * @since 0.1.0
    */
  def isMatchingCuriedRel( rel: String ): Boolean = rel startsWith curiedRelPrefix

  /**
    * Returns true, if the given link-relation type is a non-CURI rel matching the CuriTemplate
    * pattern, false if not.
    *
    * <p>
    *   Example:
    * </p>
    * <pre><code>
    *   Link curi = Link.curi("x", "http://example.org/rels/{rel}");
    *   CuriTemplate template = CuriTemplate.curiTemplateFor(curi);
    *
    *   // Returns true:
    *   template.isMatchingExpandedRel("http://example.org/rels/product");
    *
    *   // Returns false:
    *   template.isMatchingExpandedRel("x:product"):
    *   template.isMatchingExpandedRel("http://example.com/link-relations/product");
    *   template.isMatchingExpandedRel("y:product");
    * </code></pre>
    *
    * @param rel a link-relation type.
    * @return boolean
    * @since 0.1.0
    */
  def isMatchingExpandedRel( rel: String ): Boolean = {
    rel.startsWith( relPrefix ) && rel.endsWith( relSuffix )
  }

  /**
    * Returns the placeholder from a link-relation type that is matching the {@code CuriTemplate}.
    *
    * <p>
    *   Example:
    * </p>
    * <pre><code>
    *   Link curi = Link.curi("x", "http://example.org/rels/{rel}");
    *   CuriTemplate template = CuriTemplate.curiTemplateFor(curi);
    *
    *   // Returns "product":
    *   template.relPlaceHolderFrom("http://example.org/rels/product");
    *   template.relPlaceHolderFrom("x:product"):
    *
    *   // Throws IllegalArgumentException:
    *   template.relPlaceHolderFrom("http://example.com/link-relations/product");
    *   template.relPlaceHolderFrom("y:product");
    * </code></pre>
    *
    * @param rel a link-relation type.
    * @return boolean
    * @throws IllegalArgumentException if the {@code rel} parameter does not match the
    *      {@code CuriTemplate}.
    * @since 0.1.0
    */
  def relPlaceHolderFrom( rel: String ): String = {
    if (isMatchingCuriedRel( rel )) rel.substring( curiedRelPrefix.length )
    else if (isMatchingExpandedRel( rel )) {
      rel.substring( relPrefix.length, rel.length - relSuffix.length )
    } else {
      throw new IllegalArgumentException( "Rel does not match the CURI template." )
    }
  }

  /**
    * Returns the {@code rel} in CURIED format.
    *
    * <p>
    *   Example:
    * </p>
    * <pre><code>
    *   Link curi = Link.curi("x", "http://example.org/rels/{rel}");
    *   CuriTemplate template = CuriTemplate.curiTemplateFor(curi);
    *
    *   // Returns "x:product":
    *   template.curiedRelFrom("http://example.org/rels/product");
    *   template.curiedRelFrom("x:product"):
    *
    *   // Throws IllegalArgumentException:
    *   template.curiedRelFrom("http://example.com/link-relations/product");
    *   template.curiedRelFrom("y:product");
    * </code></pre>
    *
    * @param rel a link-relation type.
    * @return boolean
    * @throws IllegalArgumentException if the {@code rel} parameter does not match the {@code CuriTemplate}.
    * @since 0.1.0
    */
  def curiedRelFrom( rel: String ): String = {
    if (isMatchingCuriedRel( rel )) rel
    else if (isMatchingExpandedRel( rel )) curi.name + ":" + relPlaceHolderFrom( rel )
    else {
      throw new IllegalArgumentException( "Rel does not match the CURI template." )
    }
  }

  /**
    * Returns the {@code rel} in expanded format.
    *
    * <p>
    *   Example:
    * </p>
    * <pre><code>
    *   Link curi = Link.curi("x", "http://example.org/rels/{rel}");
    *   CuriTemplate template = CuriTemplate.curiTemplateFor(curi);
    *
    *   // Returns "http://example.org/rels/product":
    *   template.curiedRelFrom("http://example.org/rels/product");
    *   template.curiedRelFrom("x:product"):
    *
    *   // Throws IllegalArgumentException:
    *   template.curiedRelFrom("http://example.com/link-relations/product");
    *   template.curiedRelFrom("y:product");
    * </code></pre>
    *
    * @param rel a link-relation type.
    * @return boolean
    * @throws IllegalArgumentException if the {@code rel} parameter does not match the {@code CuriTemplate}.
    * @since 0.1.0
    */
  def expandedRelFrom( rel: String ): String = {
    if (isMatchingCuriedRel( rel )) {
      curi.hrefAsTemplate.set( "rel", relPlaceHolderFrom( rel ) ).expand()
    } else if (isMatchingExpandedRel( rel )) {
      rel
    } else {
      throw new IllegalArgumentException( "Rel does not match the CURI template." )
    }
  }
}

object CuriTemplate {
  val REL_PLACEHOLDER = "{rel}"

  /**
    * Creates a CuriTemplate from a CURI Link.
    *
    * <p>
    *   Example:
    * </p>
    * <pre><code>
    *   val curi = Link.curi("x", "http://example.org/rels/{rel}");
    *   val template = CuriTemplate.curiTemplateFor(curi);
    * </code></pre>
    *
    * @param curi a CURI Link
    * @return CuriTemplate
    * @throws IllegalArgumentException if the parameter is not a valid CURI.
    * @since 0.1.0
    */
  def curiTemplateFor( curi: Link ): CuriTemplate = {
    require( curi.rel != Curies.Rel || curi.name.isEmpty, "Parameter is not a CURI link." )
    require(
      !curi.href.contains( REL_PLACEHOLDER ),
      "Href of the CURI does not contain the required {rel} placeholder."
    )

    val curiHref = curi.href

    CuriTemplate(
      relPrefix = curiHref.substring( 0, curiHref.indexOf( REL_PLACEHOLDER ) ),
      curiedRelPrefix = curi.name + ":",
      relSuffix = curiHref.substring(
        curiHref.indexOf( REL_PLACEHOLDER ) + REL_PLACEHOLDER.length,
        curiHref.length
      ),
      curi = curi
    )
  }

  /**
    * Returns an optional CuriTemplate that is {@link #isMatching matching} the rel parameter, or
    * empty if no matching CURI is found.
    *
    * <p>
    *   Example:
    * </p>
    * <pre><code>
    *   List&lt;Link&gt; curies = asList(
    *        Link.curi("x", "http://example.org/rels/{rel}"),
    *        Link.curi("y", "http://example.com/link-relations/{rel}")
    *   );
    *   String rel = "y:example";
    *   CuriTemplate template = CuriTemplate.matchingCuriTemplateFor(curies, rel);
    * </code></pre>
    * <p>
    *   The returned UriTemplate is created from the second CURI, as only this is matching the
    *   curied rel parameter {@code y:example}. Because of this, {@link #expandedRelFrom(String)
    *   template.expandedRelFrom(rel)} will return {@code http://example.com/link-relations/example}
    * </p>
    * @param curies a List of curies Links
    * @param rel the link-relation type to check against the curies
    * @return optional CuriTemplate
    * @throws IllegalArgumentException if the {@code curies} parameter contains non-CURI links.
    * @since 0.1.0
    */
  def matchingCuriTemplateFor( curies: Seq[Link], rel: String ): Option[CuriTemplate] = {
    curies
      .map { curiTemplateFor }
      .filter { _ isMatching rel }
      .headOption
  }

}
