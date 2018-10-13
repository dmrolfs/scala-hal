package com.github.dmrolfs.scalahal.hal

import io.circe.Decoder.Result
import io.circe._
import io.circe.syntax._
import Links.SingleOrArray

/**
  * Representation of a number of HAL _links.
  * <p>
  *   Links can be created using the {@link Links.Builder} using the factory-method
  *   {@link Links#linkingTo()}:
  * </p>
  * <pre><code>
  *   final Links someLinks = Links.linkingTo()
  *            .self("http://example.com/shopping-cart/42")
  *            .curi("ex", "http://example.com/rels/{rel}")
  *            .item("http://example.com/products/1"),
  *            .item("http://example.com/products/2"),
  *            .single( Link.link("ex:customer", "http://example.com/customers/4711") )
  *            .build()
  *
  * </code></pre>
  *
  * @see <a href="https://tools.ietf.org/html/draft-kelly-json-hal-08#section-4.1.1">draft-kelly-json-hal-08#section-4.1.1</a>
  * @since 0.1.0
  */
case class Links private ( curies: Curies, private val _links: Map[String, SingleOrArray[Link]] ) {

  def all: Iterable[Link] = _links.values.map { _.fold( Seq( _ ), identity ) }.flatten

  /**
    * Returns a copy of this Links instance and replaces link-relation types with CURIed form, if
    * applicable.
    * <p>
    *   All CURIes are registered in the given Curies, so the HalRepresentation can forward these
    *   CURIes to embedded items.
    * </p>
    * @param curies Curies used to replace CURIed rels
    * @return Links having a reference to the given Curies.
    */
  def using( curies: Curies ): Links = this.copy( curies = curies )

  /**
    * Returns all link-relation types of the embedded items.
    *
    * @return set of link-relation types
    * @since 0.3.0
    */
  def rels: Set[String] = _links.keySet

  /**
    * <p>
    *   Returns the first (if any) link having the specified link-relation type.
    * </p>
    * <p>
    *   If CURIs are used to shorten custom link-relation types, it is possible to either use
    *   expanded link-relation types, or the CURI of the link-relation type. Using CURIs to retrieve
    *   links is not recommended, because it requires that the name of the CURI is known by clients.
    * </p>
    *
    * @param rel the link-relation type of the retrieved link.
    * @return optional link
    *
    * @see <a href="https://tools.ietf.org/html/draft-kelly-json-hal-08#section-8.2">draft-kelly-json-hal-08#section-8.2</a>
    * @since 0.1.0
    */
  def linkBy( rel: String ): Option[Link] = linksBy( rel ).headOption

  /**
    * <p>
    *   Returns the first (if any) link having the specified link-relation type and matching the given predicate.
    * </p>
    * <p>
    *   If CURIs are used to shorten custom link-relation types, it is possible to either use
    *   expanded link-relation types, or the CURI of the link-relation type. Using CURIs to retrieve
    *   links is not recommended, because it requires that the name of the CURI is known by clients.
    * </p>
    * <p>
    *   The Predicate is used to select one of possibly several links having the same link-relation
    *   type. See {@link LinkPredicates} for typical selections.
    * </p>
    *
    * @param rel the link-relation type of the retrieved link.
    * @param selector a predicate used to select one of possibly several links having the same
    *                 link-relation type
    * @return optional link
    *
    * @see <a href="https://tools.ietf.org/html/draft-kelly-json-hal-08#section-8.2">draft-kelly-json-hal-08#section-8.2</a>
    * @since 0.1.0
    */
  def linkBy( rel: String, selector: Link => Boolean ): Option[Link] = {
    linksBy( rel, selector ).headOption
  }

  /**
    * <p>
    *   Returns the list of links having the specified link-relation type.
    * </p>
    * <p>
    *   If CURIs are used to shorten custom link-relation types, it is possible to either use
    *   expanded link-relation types, or the CURI of the link-relation type. Using CURIs to retrieve
    *   links is not recommended, because it requires that the name of the CURI is known by clients.
    * </p>
    *
    * @param rel the link-relation type of the retrieved link.
    * @return list of matching link
    *
    * @see <a href="https://tools.ietf.org/html/draft-kelly-json-hal-08#section-8.2">draft-kelly-json-hal-08#section-8.2</a>
    * @since 0.1.0
    */
  def linksBy( rel: String ): Seq[Link] = {
    _links
      .get( curies.resolve( rel ) )
      .map {
        case Left( l )   => Seq( l )
        case Right( ls ) => ls
      }
      .getOrElse { Seq.empty[Link] }
  }

  /**
    * <p>
    *   Returns the list of links having the specified link-relation type and matching the given
    *   predicate.
    * </p>
    * <p>
    *   If CURIs are used to shorten custom link-relation types, it is possible to either use
    *   expanded link-relation types, or the CURI of the link-relation type. Using CURIs to retrieve
    *   links is not recommended, because it requires that the name of the CURI is known by clients.
    * </p>
    * <p>
    *   The Predicate is used to select some of possibly several links having the same link-relation
    *   type. See {@link LinkPredicates} for typical selections.
    * </p>
    *
    * @param rel the link-relation type of the retrieved link.
    * @param selector a predicate used to select some of the links having the specified
    *                 link-relation type
    * @return list of matching link
    *
    * @see <a href="https://tools.ietf.org/html/draft-kelly-json-hal-08#section-8.2">draft-kelly-json-hal-08#section-8.2</a>
    * @since 0.1.0
    */
  def linksBy( rel: String, selector: Link => Boolean ): Seq[Link] =
    linksBy( rel ).filter( selector )

  /**
    * Removes the links with link-relation type {@code rel} from the Links object.
    * @param rel link-relation type
    */
  def remove( rel: String ): Links = this.copy( _links = this._links - rel )

  /**
    * Checks the existence of a link with link-relation type {@code rel}
    * @param rel the link-relation type
    * @return true, if links exists, false otherwise
    *
    * @since 0.1.0
    */
  def hasLink( rel: String ): Boolean = _links contains rel

  /**
    * Returns true if there is at least one link with link-relation type {@code rel}, and if the
    * link will be rendered as an array of link-objects.
    *
    * @param rel the link-relation type
    * @return boolean
    *
    * @since 0.1.0
    */
  def isArray( rel: String ): Boolean = {
    _links
      .get( rel )
      .map { _.isRight }
      .getOrElse { false }
  }

  /**
    *
    * @return true if Links is empty, false otherwise.
    *
    * @since 0.1.0
    */
  def isEmpty: Boolean = _links.isEmpty
}

object Links {

  type SingleOrArray[T] = Either[T, Seq[T]]
  type RelLink = ( String, SingleOrArray[Link] )

  val empty: Links = Links( curies = Curies.empty, _links = Map.empty[String, SingleOrArray[Link]] )

  def linkingTo: Builder = Builder()

  /**
    * Factory method used to build a Links.Builder that is initialized from a prototype Links
    * instance.
    *
    * @param prototype the prototype used to initialize the builder
    * @return Links.Builder
    */
  def fromPrototype( prototype: Links ): Builder = Builder().withLinks( prototype )

  /**
    * <p>
    *   Creates a Links object from a map containing rel->List<Link>.
    * </p>
    * <p>
    *   If the links contain curies, the link-relation types are shortened to the curied format
    *   name:key.
    * </p>
    * <p>
    *   The list of links for a link-relation type must have the same {@link Link#rel}
    * </p>
    * <p>
    *   The {@link Curies} may contain CURIs from the root resource, so curied rels can be resolved.
    * </p>
    *
    * @param links a map with link-relation types as key and the list of links as value.
    * @param curies the Curies used to CURI the link-relation types of the links.
    * @since 0.1.0
    */
//  private def unsafeCreate( links: Map[String, Seq[Link]], curies: Curies ) = {
//    val curiLinks = links.getOrElse( Curies.Rel, Seq.empty[Link] )
//    val registeredCuries = curiLinks.foldLeft( curies ) { _ register _ }
//    val resolvedLinks = links.keySet.foldLeft( Map.empty[String, Seq[Link]] ) { ( acc, rel ) =>
//      acc + (registeredCuries.resolve( rel ) -> links( rel ))
//    }
//
//    Links( links = resolvedLinks, curies = registeredCuries )
//  }

  /**
    * A Builder used to build Links instances.
    *
    * @since 0.1.0
    */
  case class Builder(
    curies: Curies = Curies.empty,
    _links: Map[String, SingleOrArray[Link]] = Map.empty[String, SingleOrArray[Link]],
  ) {

    /**
      * Adds a 'self' link and returns the Builder.
      * <p>
      *   Using this method is equivalent to {@link #single(Link, Link...) single(Link.self(href))}
      * </p>
      *
      * @param href href of the linked resource
      * @return Builder
      *
      * @see <a href="http://www.iana.org/assignments/link-relations/link-relations.xhtml">IANA link-relations</a>
      * @since 0.1.0
      */
    def self( href: String ): Builder = single( Link self href )

    /**
      * <p>
      *   Adds a 'curies' link (compact URI) with name and a URI template for the link-relation
      *   type.
      * </p>
      * <p>
      *   Curies may be used for brevity for custom link-relation type URIs. Curies are established
      *   within a HAL document via a set of Link Objects with the relation type "curies" on the
      *   root Resource Object. These links contain a URI template with the token 'rel', and are
      *   named via the "name" property.
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
      * <p>
      *   Using this method is equivalent to
      *   {@link #array(Link, Link...) array(Link.curi(name, relTemplate))}
      * </p>
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
    def curi( name: String, relTemplate: String ): Builder = {
      array(
        Link.curi(
          name = name,
          relTemplate = relTemplate
        )
      )
    }

    /**
      * Adds an 'item' link to the builder.
      * <p>
      *   If href is an URI template, the added link {@link Link#isTemplated() is templated}.
      * </p>
      * <p>
      *   Using this method is equivalent to {@link #array(Link, Link...) array(Link.item(href))}
      * </p>
      *
      * @param href the linked item
      * @return this
      *
      * @see <a href="http://www.iana.org/assignments/link-relations/link-relations.xhtml">IANA link-relations</a>
      * @since 0.1.0
      */
    def item( href: String ): Builder = array( Link item href )

    /**
      * Adds one or more links to the builder that will be rendered as a single link-object instead
      * of an array of link-objects.
      * <p>
      *   The links must have different {@link Link#getRel() Link-Relation Types}, otherwise it
      *   would not be possible to render them as single link-objects. If two or more links have the
      *   same Link-Relation Type, an IllegalArgumentException is thrown.
      * </p>
      * <p>
      *   Because curies must always be {@link #array(List) array} links, it is not possible to add
      *   links with {@code rel='curies'} to the builder using {@link #single(Link, Link...)} or
      *   {@link #single(List)}.
      * </p>
      * <p>
      *   As specified in <a href="https://tools.ietf.org/html/draft-kelly-json-hal-06#section-4.1.1">Section 4.1.1</a>
      *   of the HAL specification, the {@code _links} object <em>"is an object whose property names
      *   are link relation types (as defined by [RFC5988]) and values are either a Link Object or
      *   an array of Link Objects"</em>.
      * </p>
      * <p>
      *   Adding a link using {@code single(Link)} will result in a representation, where the link
      *   is rendered as a Link Object.
      * </p>
      * <p>
      *   Calling {@code single(Link)} with a {@link Link#getRel() link-relation type} that is
      *   already present, an {@link IllegalStateException} is thrown.
      * </p>
      *
      * @param link the added link. The Link-Relation Type of the link must not yet be added to the
      *             builder.
      * @param more optionally more links having different Link-Relation Types
      * @return this
      * @throws IllegalStateException if the Link-Relation Type of the link is already associated
      *                               with another Link.
      *
      * @see <a href="http://www.iana.org/assignments/link-relations/link-relations.xhtml">IANA link-relations</a>
      * @since 0.1.0
      */
    def single( link: Link, more: Link* ): Builder = {
      require(
        link.rel != Curies.Rel,
        "According to the spec, curies must always be arrays of links, not single links."
      )

      val newBuilder = if (!this._links.contains( link.rel )) {
        this.copy( _links = this._links + (link.rel -> Left( link )) )
      } else {
        throw new IllegalStateException(
          s"The Link-Relation Type '${link.rel}' of the Link is already present."
        )
      }

      more.foldLeft( newBuilder ) { ( acc, m ) =>
        acc.single( m )
      }
    }

    /**
      * Adds a list of links to the builder that will be rendered as a single link-object instead of
      * an array of link-objects.
      * <p>
      *   As specified in <a href="https://tools.ietf.org/html/draft-kelly-json-hal-06#section-4.1.1">Section 4.1.1</a>
      *   of the HAL specification, the {@code _links} object <em>"is an object whose property names
      *   are link relation types (as defined by [RFC5988]) and values are either a Link Object or
      *   an array of Link Objects"</em>.
      * </p>
      * <p>
      *   Adding a link using {@code single(Link)} will result in a representation, where the link
      *   is rendered as a Link Object.
      * </p>
      * <p>
      *   The List of links must not contain multiple links having the same link-relation type,
      *   otherwise an IllegalArgumentException is thrown.
      * </p>
      * <p>
      *   Because curies must always be {@link #array(List) array} links, it is not possible to add
      *   links with {@code rel='curies'} to the builder using {@link #single(Link, Link...)} or
      *   {@link #single(List)}.
      * </p>
      * <p>
      *   Calling {@code single(List<Link>)} with {@link Link#getRel() link-relation types} that are
      *   already present, an {@link IllegalStateException} is thrown.
      * </p>
      *
      * @param singleLinkObjects the added link. The Link-Relation Type of the link must not yet be
      *                          added to the builder.
      * @return this
      * @throws IllegalArgumentException if the list contains multiple links having the same
      *                                  Link-Relation Type.
      * @throws IllegalStateException if the Link-Relation Type of the link is already associated
      *                               with another Link.
      *
      * @see <a href="http://www.iana.org/assignments/link-relations/link-relations.xhtml">IANA link-relations</a>
      * @since 0.1.0
      */
    def single( singleLinks: Seq[Link] ): Builder = {
      require(
        singleLinks.map { _.rel }.distinct.size == singleLinks.size,
        "Unable to add links as single link objects as there are multiple links having the same " +
        "link-relation type."
      )
      singleLinks.foldLeft( this ) { ( acc, l ) =>
        acc.single( l )
      }
    }

    /**
      * Adds one or more Links.
      * <p>
      *   {@link Link#isEquivalentTo(Link) Equivalent} links are NOT added but silently ignored.
      * </p>
      * <p>
      *   As specified in <a href="https://tools.ietf.org/html/draft-kelly-json-hal-06#section-4.1.1">Section 4.1.1</a>
      *   of the HAL specification, the {@code _links} object <em>"is an object whose property names
      *   are link relation types (as defined by [RFC5988]) and values are either a Link Object or
      *   an array of Link Objects"</em>.
      * </p>
      * <p>
      *   Adding a link using {@code array(Link, Link...)} will result in a representation, where
      *   the links are rendered as an array of Link Objects, even if there are only single links
      *   for a given Link-Relation Type.
      * </p>
      *
      * @param link a Link
      * @param more more links
      * @return this
      *
      * @since 0.1.0
      */
    def array( link: Link, more: Link* ): Builder = array( link +: more )

    /**
      * Adds a list of links to the builder that will  be rendered as an array of link-objects
      * <p>
      *   {@link Link#isEquivalentTo(Link) Equivalent} links are NOT added but silently ignored.
      * </p>
      * <p>
      *   As specified in <a href="https://tools.ietf.org/html/draft-kelly-json-hal-06#section-4.1.1">Section 4.1.1</a>
      *   of the HAL specification, the {@code _links} object <em>"is an object whose property names
      *   are link relation types (as defined by [RFC5988]) and values are either a Link Object or
      *   an array of Link Objects"</em>.
      * </p>
      * <p>
      *   Adding a link using {@code array(List<Link>)} will result in a representation, where the
      *   links are rendered as an array of Link Objects, even if there are only single links for a
      *   given Link-Relation Type.
      * </p>
      *
      * @param links the list of links.
      * @return this
      *
      * @since 0.1.0
      */
    def array( links: Seq[Link] ): Builder = {
      links.foldLeft( this ) { ( acc, link ) =>
        val preppedLinks = {
          if (acc._links contains link.rel) acc._links
          else acc._links + (link.rel -> Right( Seq.empty[Link] ))
        }

        val linksForRel = {
          val rl: SingleOrArray[Link] = preppedLinks( link.rel )
          rl match {
            case Right( ls ) => {
              val equivalentLinkExists = ls exists { _ isEquivalentTo link }
              if (!equivalentLinkExists) ls :+ link else ls
            }

            case Left( _ ) => {
              throw new IllegalStateException(
                s"Unable to add links with rel=[${link.rel}] as there is already a single Link " +
                s"Object added for this link-relation type"
              )
            }
          }
        }

        val newLinks = preppedLinks + (link.rel -> Right( linksForRel ))
        acc.copy( _links = newLinks )
      }
    }

    /**
      * Replaces the link(s) currently associated with {@code rel} by a list of links to the builder
      * that will  be rendered as an array of link-objects.
      * <p>
      *   All links must have the {@link Link#getRel() Link-Relation Type} specified in {@code rel},
      *   otherwise an {@link IllegalArgumentException} is thrown.
      * </p>
      * <p>
      *   As specified in <a href="https://tools.ietf.org/html/draft-kelly-json-hal-06#section-4.1.1">Section 4.1.1</a>
      *   of the HAL specification, the {@code _links} object <em>"is an object whose property names
      *   are link relation types (as defined by [RFC5988]) and values are either a Link Object or
      *   an array of Link Objects"</em>.
      * </p>
      * <p>
      *   Replacing links using {@code replace(List<Link>)} will result in a representation, where
      *   the links are rendered as an array of Link Objects, even if there are only single links
      *   for a given Link-Relation Type.
      * </p>
      *
      * @param rel the Link-Relation Type that is replaced by the links
      * @param links the list of links.
      * @return this
      *
      * @since 0.1.0
      */
    def replace( rel: String, links: Seq[Link] ): Builder = {
      require( links.exists { _.rel == rel }, s"All links must have link-relation type ${rel}" )
      if (!this._links.contains( rel )) array( links )
      else this.copy( _links = this._links + (rel -> Right( links )) )
    }

    /**
      * Adds all links from {@link Links} to the builder.
      * <p>
      *   {@link Link#isEquivalentTo(Link) Equivalent} links are NOT added but silently ignored in
      *   order to avoid duplicate links.
      * </p>
      *
      * @param moreLinks the added links.
      * @return this
      *
      * @since 0.1.0
      */
    def withLinks( moreLinks: Links ): Builder = {
      moreLinks.rels.foldLeft( this ) { ( acc, rel ) =>
        if (moreLinks.isArray( rel )) array( moreLinks linksBy rel )
        else single( moreLinks linksBy rel )
      }
    }

    /**
      * Removes registered links from the builder.
      *
      * @param rel Link-Relation Type of the links that should be removed.
      * @return this
      *
      * @since 0.1.0
      */
    def without( rel: String ): Builder = this.copy( _links = this._links - rel )

    def using( curies: Curies ): Builder = this.copy( curies = this.curies.mergeWith( curies ) )

    /**
      * Creates a Links instance from all added links.
      *
      * @return Links
      */
    def build(): Links = Links( curies, _links )
  }

  /**
    * A Circe json Encoder for Links. Used to render the _links part of HAL+JSON documents.
    *
    * @since 0.1.0
    */
  implicit val encoder: Encoder[Links] = new Encoder[Links] {
    override def apply( links: Links ): Json = {
      val fields: Map[String, Json] = for {
        relLinks <- links._links
        ( rel, ls ) = relLinks
      } yield {
        val linkJsons = ls.fold( l => Left( l.asJson ), ls => Right( ls.map( _.asJson ) ) )
        val value = linkJsons.fold( identity, Json.fromValues )
        ( rel, value )
      }

      Json.obj( fields.toSeq: _* )
    }
  }

  /**
    * A Circe Decoder for Links. Used to parse the _links part of HAL+JSON documents.
    *
    * @since 0.1.0
    */
  implicit val decoder: Decoder[Links] = new Decoder[Links] {
    override def apply( c: HCursor ): Result[Links] = {
      import cats.instances.either._
      import cats.instances.list._
      import cats.syntax.traverse._

      val parsedLinks: Iterable[Decoder.Result[RelLink]] = {
        for {
          ks <- c.keys.toIterable
          k  <- ks
        } yield {
          c.downField( k )
            .focus
            .map {
              case json if json.isArray => json.as[Seq[Link]] map { Right( _ ) }
              case json                 => json.as[Link] map { Left( _ ) }
            }
            .map { values =>
              values map { ( k, _ ) }
            }
            .getOrElse {
//              val result: Decoder.Result[RelLink] = {
              Right( ( k, Right( Seq.empty[Link] ) ) )
//              }
//              result
            }
//            .getOrElse {
//              Right( Right( Seq.empty[Link]))
//            }

//          //todo need to parse either single or Seq[Link]
//          c.downField( k ).as[Seq[Link]] map { values =>
//            ( k, values )
//          }
        }
      }

      parsedLinks.toList.sequence.map { relLinks =>
        val all: Map[String, SingleOrArray[Link]] = Map( relLinks: _* )
        val curies = {
          all
            .get( Curies.Rel )
            .map { _.fold( Seq( _ ), identity ) }
            .map { Curies.apply }
            .getOrElse { Curies.empty }
        }
        Links.Builder( curies, all - Curies.Rel ).build()
      }
    }
  }
//  public static class LinksDeserializer extends JsonDeserializer<Links> {
//
//
//    private static final TypeReference<Map<String, ?>> TYPE_REF_LINK_MAP = new TypeReference<Map<String, ?>>() {};
//
//    /**
//      * {@inheritDoc}
//      */
//    @Override
//    public Links deserialize(JsonParser p, DeserializationContext ctxt) throws IOException {
//      final Map<String,?> linksMap = p.readValueAs(TYPE_REF_LINK_MAP);
//      final Map<String, Object> links = linksMap
//        .entrySet()
//        .stream()
//        .collect(toMap(Map.Entry::getKey, e -> asArrayOrObject(e.getKey(), e.getValue())));
//
//      fixPossibleIssueWithCuriesAsSingleLinkObject(links);
//
//      return new Links(
//        links,
//        emptyCuries()
//      );
//    }
//
//    private void fixPossibleIssueWithCuriesAsSingleLinkObject(Map<String, Object> links) {
//      // CURIES should always have a List of Links. Because this might not aways be the case, we have to fix this:
//      if (links.containsKey(CURIES_REL)) {
//        if (links.get(CURIES_REL) instanceof Link) {
//          links.put(CURIES_REL, new ArrayList<Link>() {{
//            add((Link) links.get(CURIES_REL));
//          }});
//        }
//      }
//    }
//
//    @SuppressWarnings({"unchecked", "rawtypes"})
//    private Object asArrayOrObject(final String rel, final Object value) {
//      if (value instanceof Map) {
//        return asLink(rel, (Map)value);
//      } else {
//        try {
//          return ((List<Map>) value).stream().map(o -> asLink(rel, o)).collect(toList());
//        } catch (final ClassCastException e) {
//          throw new IllegalStateException("Expected a single Link or a List of Links: rel=" + rel + " value=" + value);
//        }
//      }
//    }
//
//    @SuppressWarnings("rawtypes")
//    private Link asLink(final String rel, final Map value) {
//      Link.Builder builder = linkBuilder(rel, value.get("href").toString())
//        .withHrefLang((String) value.get("hreflang"))
//      .withName((String) value.get("name"))
//      .withTitle((String) value.get("title"))
//      .withType((String) value.get("type"))
//      .withProfile((String) value.get("profile"))
//      .withDeprecation((String) value.get("deprecation"));
//      return builder.build();
//    }
//  }

}
