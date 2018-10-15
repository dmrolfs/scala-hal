package com.github.dmrolfs.scalahal.hal

import com.damnhandy.uri.template.UriTemplate
import io.circe.{ Decoder, Encoder, HCursor, Json }

case class Link private (
  /**
    * Returns the link-relation type of the link.
    *
    * @return link-relation type
    * @since 0.1.0
    */
  rel: String,
  /**
    * Returns the href of the link.
    * <p>
    * The "href" property is REQUIRED.
    * </p>
    * <p>
    * Its value is either a URI [RFC3986] or a URI Template [RFC6570]. Templates can
    * be expanded by creating an {@link com.damnhandy.uri.template.UriTemplate}.
    * </p>
    * <p>
    * If the value is a URI Template then the Link Object SHOULD have a
    * "templated" attribute whose value is true.
    * </p>
    *
    * @return href of the linked resource, or URI template, if the link is {@code templated}.
    * @see <a href="https://tools.ietf.org/html/draft-kelly-json-hal-08#section-5.1">draft-kelly-json-hal-08#section-5.1</a>
    * @since 0.1.0
    */
  href: String,
  /**
    * Returns the type of the link, or an empty String if no type is specified.
    * <p>
    * The "type" property is OPTIONAL.
    * </p>
    * <p>
    * Its value is a string used as a hint to indicate the media type
    * expected when dereferencing the target resource.
    * </p>
    *
    * @return type
    * @see <a href="https://tools.ietf.org/html/draft-kelly-json-hal-08#section-5.3">draft-kelly-json-hal-08#section-5.3</a>
    * @since 0.1.0
    */
  `type`: Option[String] = None,
  /**
    * Returns the hreflang of the link, or an empty String if no hreflang is specified.
    * <p>
    * The "hreflang" property is OPTIONAL.
    * </p>
    * <p>
    * Its value is a string and is intended for indicating the language of
    * the target resource (as defined by [RFC5988]).
    * </p>
    *
    * @return hreflang or empty string
    * @see <a href="https://tools.ietf.org/html/draft-kelly-json-hal-08#section-5.8">draft-kelly-json-hal-08#section-5.8</a>
    * @since 0.1.0
    */
  hreflang: Option[String] = None,
  /**
    * Returns the title of the link, or an empty String if no title is specified.
    * <p>
    * The "title" property is OPTIONAL.
    * </p>
    * <p>
    * Its value is a string and is intended for labelling the link with a
    * human-readable identifier (as defined by [RFC5988]).
    * </p>
    *
    * @return title or empty string
    * @see <a href="https://tools.ietf.org/html/draft-kelly-json-hal-08#section-5.7">draft-kelly-json-hal-08#section-5.7</a>
    * @since 0.1.0
    */
  title: Option[String] = None,
  /**
    * Returns the name of the link, or an empty String if no name is specified.
    * <p>
    * The "name" property is OPTIONAL.
    * </p>
    * <p>
    * Its value MAY be used as a secondary key for selecting Link Objects
    * which share the same relation type.
    * </p>
    *
    * @return name or empty string
    * @see <a href="https://tools.ietf.org/html/draft-kelly-json-hal-08#section-5.5">draft-kelly-json-hal-08#section-5.5</a>
    * @since 0.1.0
    */
  name: Option[String] = None,
  /**
    * Returns the deprecation information, or an empty string, if the link is not deprecated.
    * <p>
    * The "deprecation" property is OPTIONAL.
    * </p>
    * <p>
    * Its presence indicates that the link is to be deprecated (i.e.
    * removed) at a future date.  Its value is a URL that SHOULD provide
    * further information about the deprecation.
    * </p>
    * <p>
    * A client SHOULD provide some notification (for example, by logging a
    * warning message) whenever it traverses over a link that has this
    * property.  The notification SHOULD include the deprecation property's
    * value so that a client manitainer can easily find information about
    * the deprecation.
    * </p>
    *
    * @return URL
    * @see <a href="https://tools.ietf.org/html/draft-kelly-json-hal-08#section-5.4">draft-kelly-json-hal-08#section-5.4</a>
    * @since 0.1.0
    */
  //todo change to URL
  deprecation: Option[String] = None,
  /**
    * Returns the profile of the link, or an empty String if no profile is specified.
    * <p>
    * The "profile" property is OPTIONAL.
    * </p>
    * <p>
    * Its value is a string which is a URI that hints about the profile of the target resource.
    * </p>
    *
    * @return profile or empty string
    * @see <a href="https://tools.ietf.org/html/draft-kelly-json-hal-08#section-5.6">draft-kelly-json-hal-08#section-5.6</a>
    * @since 0.1.0
    */
  profile: Option[String] = None
) {

  /**
    * Returns true, if the link is templated, false otherwise.
    * <p>
    * The "templated" property is OPTIONAL.
    * </p>
    * <p>
    * Its value is boolean and SHOULD be true when the Link Object's "href"
    * property is a URI Template.
    * </p>
    * <p>
    * Its value SHOULD be considered false if it is undefined or any other
    * value than true.
    * </p>
    *
    * @return boolean
    * @see <a href="https://tools.ietf.org/html/draft-kelly-json-hal-08#section-5.2">draft-kelly-json-hal-08#section-5.2</a>
    * @since 0.1.0
    */
  val templated: Boolean = UriTemplate.fromTemplate( href ).getVariables.length > 0

  /**
    * Returns the href of the link as a {@link UriTemplate} that can be expanded by providing the
    * required template variables.
    *
    * @return uri template of the linked resource
    *
    * @see <a href="https://tools.ietf.org/html/draft-kelly-json-hal-08#section-5.1">draft-kelly-json-hal-08#section-5.1</a>
    * @since 0.1.0
    */
  def hrefAsTemplate: UriTemplate = UriTemplate fromTemplate href

  /**
    * Two links are considerered equivalent, if they have the same
    * {@link #getRel() link-relation type} and are pointing to the {@link #getHref() same resource}
    * in the same {@link #getType() representation} and {@link #getProfile() profile}.
    *
    * @param other other link
    * @return true if the links are equivalent, false otherwise.
    */
  def isEquivalentTo( that: Link ): Boolean = {
    (that.rel == this.rel) &&
    (that.href == this.href) &&
    (that.`type` == this.`type`) &&
    (that.profile == this.profile)
  }

  override def toString: String = {
    val fields = Seq(
      Some( ( "rel", rel ) ),
      Some( ( "href", href ) ),
      Some( ( "templated", templated ) ),
      `type` map { v =>
        ( "type", v )
      },
      hreflang map { v =>
        ( "hreflang", v )
      },
      title map { v =>
        ( "title", v )
      },
      name map { v =>
        ( "name", v )
      },
      profile map { v =>
        ( "profile", v )
      },
      deprecation map { v =>
        ( "deprecation", v )
      }
    )

    "Link" + fields.flatten.map { case ( k, v ) => k + "=" + v }.mkString( "(", ", ", ")" )
  }
}

object Link {

  /**
    * Create a 'self' link from a href.
    *
    * @param href href of the linked resource
    * @return Link
    * @see <a href="http://www.iana.org/assignments/link-relations/link-relations.xhtml">IANA link-relations</a>
    */
  def self( href: String ): Link = Link( rel = "self", href = href )

  /**
    * <p>
    *   Create a 'curies' link (compact URI) with name and a URI template for the link-relation type.
    * </p>
    * <p>
    *   Curies may be used for brevity for custom link-relation type URIs. Curiess are established
    *   within a HAL document via a set of Link Objects with the relation type "curies" on the root
    *   Resource Object. These links contain a URI template with the token 'rel', and are named via
    *   the "name" property.
    * </p>
    * <pre><code>
    *   {
    *     "_links": {
    *       "self": { "href": "/orders" },
    *       "curies": [{
    *         "name": "acme",
    *         "href": "http://docs.acme.com/relations/{rel}",
    *         "templated": true
    *       }],
    *       "acme:widgets": { "href": "/widgets" }
    *     }
    *   }
    * </code></pre>
    *
    * @param name the short name of the CURI
    * @param relTemplate the template used to build link-relation types. Must contain a {rel}
    *                    placeholder
    * @return Link
    *
    * @see <a href="http://www.iana.org/assignments/link-relations/link-relations.xhtml">IANA link-relations</a>
    * @see <a href="https://tools.ietf.org/html/draft-kelly-json-hal-08#section-8.2">draft-kelly-json-hal-08#section-8.2</a>
    * @since 0.1.0
    */
  def curi( name: String, relTemplate: String ): Link = {
    require(
      relTemplate.contains( "{rel}" ),
      "Not a CURI template. Template is required to contain a {rel} placeholder"
    )

    Link( rel = "curies", href = relTemplate, name = Some( name ) )
  }

  /**
    * Create a 'profile' link from a href
    *
    * @param href the linked profile
    * @return Link
    *
    * @see <a href="http://www.iana.org/assignments/link-relations/link-relations.xhtml">IANA link-relations</a>
    * @since 0.1.0
    */
  def profile( href: String ): Link = Link( rel = "profile", href = href )

  /**
    * Create a 'item' link from a href.
    * <p>
    *   If href is an URI template, the created link {@link #isTemplated() is templated} and
    *   {@link #templated} will be true.
    * </p>
    *
    * @param href the linked item
    * @return Link
    *
    * @see <a href="http://www.iana.org/assignments/link-relations/link-relations.xhtml">IANA link-relations</a>
    * @since 0.1.0
    */
  def item( href: String ): Link = Link( rel = "item", href = href )

  /**
    * Create a 'collection' link from a href
    * <p>
    *   If href is an URI template, the created link {@link #isTemplated() is templated} and
    *   {@link #templated} will be true.
    * </p>
    *
    * @param href the linked collection
    * @return Link
    *
    * @see <a href="http://www.iana.org/assignments/link-relations/link-relations.xhtml">IANA link-relations</a>
    * @since 0.1.0
    */
  def collection( href: String ): Link = Link( rel = "collection", href = href )

  /**
    * Create a link from a link-relation type and href.
    * <p>
    *   If href is an URI template, the created link {@link #isTemplated() is templated} and
    *   {@link #templated} will be true.
    * </p>
    *
    * @param rel registered link-relation type, or URI identifying a custom link-relation type.
    * @param href href of the linked resource
    * @return Link
    *
    * @see <a href="http://www.iana.org/assignments/link-relations/link-relations.xhtml">IANA link-relations</a>
    * @since 0.1.0
    */
  def link( rel: String, href: String ): Link = Link( rel = rel, href = href )

  /**
    * Create a Builder instance with mandatory link-relation type and href
    * <p>
    *   If href is an URI template, the link created by the Builder will be {@link #isTemplated()
    *   templated}.
    * </p>
    *
    * @param rel  the link-relation type of the link
    * @param href the href of the linked resource
    * @return a Builder for a Link.
    *
    * @since 0.1.0
    */
  def linkBuilder( rel: String, href: String ): Builder = Builder( rel = rel, href = href )

  /**
    * Create a Builder instance and initialize it from a prototype Link.
    *
    * @param prototype the prototype link
    * @return a Builder for a Link.
    *
    * @since 0.1.0
    */
  def fromPrototype( prototype: Link ): Builder = {
    val b1 = Builder( rel = prototype.rel, href = prototype.href )
    val b2 = prototype.`type` map { b1.withType } getOrElse { b1 }
    val b3 = prototype.profile map { b2.withProfile } getOrElse { b2 }
    val b4 = prototype.title map { b3.withTitle } getOrElse { b3 }
    val b5 = prototype.name map { b4.withName } getOrElse { b4 }
    val b6 = prototype.deprecation map { b5.withDeprecation } getOrElse { b5 }
    val b7 = prototype.hreflang map { b6.withHrefLang } getOrElse { b6 }
    b7
  }

  /**
    * A Builder used to build complex links.
    *
    * @param rel  the link-relation type of the link
    * @param href the href of the linked resource
    * @since 0.1.0
    */
  case class Builder(
    rel: String,
    href: String,
    `type`: Option[String] = None,
    hreflang: Option[String] = None,
    title: Option[String] = None,
    name: Option[String] = None,
    profile: Option[String] = None,
    deprecation: Option[String] = None
  ) {

    /**
      * Set the rel of the linked resource
      *
      * @param rel link-relation type
      * @return Builder
      *
      * @since 0.1.0
      */
    def withRel( rel: String ): Builder = {
      if (rel.isEmpty) throw new IllegalArgumentException( "The link-relation type is mandatory" )
      this.copy( rel = rel )
    }

    /**
      * Set the href of the linked resource
      * <p>
      *   If href is an URI template, the created link {@link #isTemplated() is templated} and
      *   {@link #templated} will be true.
      * </p>
      *
      * @param href href
      * @return Builder
      *
      * @since 0.1.0
      */
    def withHref( href: String ): Builder = {
      if (href.isEmpty) throw new IllegalArgumentException( "The href parameter is mandatory" )
      this.copy( href = href )
    }

    /**
      * Set the media type of the linked resource
      *
      * @param type media type
      * @return Builder
      *
      * @since 0.1.0
      */
    def withType( mediaType: String ): Builder = {
      this.copy( `type` = Option( mediaType ).filter { !_.isEmpty } )
    }

    /**
      * Set the language of the linked resource
      *
      * @param hrefLang the hreflang of the Link
      * @return Builder
      * @since 0.1.0
      */
    def withHrefLang( hrefLang: String ): Builder = {
      this.copy( hreflang = Option( hrefLang ).filter { !_.isEmpty } )
    }

    /**
      * Set the title attribute
      *
      * @param title the title of the linked resource.
      * @return Builder
      *
      * @since 0.1.0
      */
    def withTitle( title: String ): Builder = {
      this.copy( title = Option( title ).filter { !_.isEmpty } )
    }

    /**
      * Set the name attribute.
      *
      * @param name the name of the linked resource.
      * @return Builder
      *
      * @since 0.1.0
      */
    def withName( name: String ): Builder = {
      this.copy( name = Option( name ).filter { !_.isEmpty } )
    }

    /**
      * Set the profile attribute
      *
      * @param profile the profile of the representation
      * @return Builder
      *
      * @since 0.1.0
      */
    def withProfile( profile: String ): Builder = {
      this.copy( profile = Option( profile ).filter { !_.isEmpty } )
    }

    /**
      * <p>
      *   Set deprecation attribute.
      * </p>
      * <p>
      *   Its presence indicates that the link is to be deprecated (i.e. removed) at a future date.
      *   Its value is a URL that SHOULD provide further information about the deprecation.
      * </p>
      *
      * @param deprecation URL pointing to further information
      * @return Builder
      * @since 0.1.0
      */
    def withDeprecation( deprecation: String ): Builder = {
      this.copy( deprecation = Option( deprecation ).filter { !_.isEmpty } )
    }

    /**
      * Builds the Link instance.
      *
      * @return Link
      *
      * @since 0.1.0
      */
    def build(): Link = {
      Link(
        rel = this.rel,
        href = this.href,
        `type` = this.`type`,
        hreflang = this.hreflang,
        title = this.title,
        name = this.name,
        deprecation = this.deprecation,
        profile = this.profile
      )
    }
  }

  implicit val encoder: Encoder[Link] = new Encoder[Link] {
    override def apply( l: Link ): Json = {
      val fields: Seq[Option[( String, Json )]] = Seq(
        Some( ( "href", Json.fromString( l.href ) ) ),
        Some( ( "templated", Json.fromBoolean( l.templated ) ) ),
        l.`type` map { v =>
          ( "type", Json.fromString( v ) )
        },
        l.hreflang map { v =>
          ( "hreflang", Json.fromString( v ) )
        },
        l.title map { v =>
          ( "title", Json.fromString( v ) )
        },
        l.name map { v =>
          ( "name", Json.fromString( v ) )
        },
        l.deprecation map { v =>
          ( "deprecation", Json.fromString( v ) )
        },
        l.profile map { v =>
          ( "profile", Json.fromString( v ) )
        }
      )

      Json.obj( fields.flatten: _* )
    }
  }

  implicit val decode: Decoder[Link] = new Decoder[Link] {
    override def apply( c: HCursor ): Decoder.Result[Link] = {
      for {
        href        <- c.downField( "href" ).as[String]
        templated   <- c.downField( "templated" ).as[Boolean]
        mediaType   <- c.downField( "type" ).as[Option[String]]
        deprecation <- c.downField( "deprecation" ).as[Option[String]]
        name        <- c.downField( "name" ).as[Option[String]]
        profile     <- c.downField( "profile" ).as[Option[String]]
        title       <- c.downField( "title" ).as[Option[String]]
        hreflang    <- c.downField( "hreflang" ).as[Option[String]]
      } yield {
        val result = Link(
          rel = "",
          href = href,
          `type` = mediaType,
          deprecation = deprecation, //todo .map { new URL( _ ) },
          name = name,
          profile = profile,
          title = title,
          hreflang = hreflang
        )

        if (result.templated != templated) {
          throw new IllegalStateException(
            s"Link Json templated[${templated}] doesn't match instantiated " +
            s"Link's[${result.templated}]"
          )
        }

        result
      }
    }
  }

}
