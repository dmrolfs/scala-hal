package com.github.dmrolfs.scalahal.hal

import com.github.dmrolfs.scalahal.{ HalDecoder, HalEncoder }
import io.circe.Decoder.Result
import io.circe._
import io.circe.syntax._

/**
  * Representation used to parse and create HAL+JSON documents from Java classes.
  *
  * @see <a href="http://stateless.co/hal_specification.html">hal_specification.html</a>
  * @see <a href="https://tools.ietf.org/html/draft-kelly-json-hal-08">draft-kelly-json-hal-08</a>
  * @since 0.1.0
  */
case class HalRepresentation private (
  /**
    * The Links of the HalRepresentation.
    *
    * @return Links
    * @since 0.1.0
    */
  links: Links,
  /**
    * The Embedded objects of the HalRepresentation.
    *
    * @return Embedded, possibly beeing {@link Embedded#isEmpty() empty}
    */
  embedded: Embedded,
  /**
    * Extra attributes that were not mapped to properties of the HalRepresentation.
    *
    * @return map containing unmapped attributes
    */
  attributes: Map[String, String], // (String -> Json)
  /**
    * @return the Curies used by this HalRepresentation.
    */
  curies: Curies
) {

  /**
    * Returns the value of an extra attribute as a JsonNode, or null if no such attribute is present.
    *
    * @param name the name of the attribute
    * @return JsonNode or null
    */
  def attribute( name: String ): Option[String] = attributes.get( name )

  def addJsonAttribute( name: String, jsonValue: String ): HalRepresentation = {
    this.copy( attributes = this.attributes + (name -> jsonValue) )
  }

  def addAttribute[T: Encoder]( name: String, value: T ): HalRepresentation = {
    addJsonAttribute( name = name, jsonValue = value.asJson.noSpaces )
  }

  /**
    * Add links to the HalRepresentation.
    * <p>
    *   Links are only added if they are not {@link Link#isEquivalentTo(Link) equivalent} to already
    *   existing links.
    * </p>
    * @param links links that are added to this HalRepresentation
    * @return this
    */
  def add( links: Links ): HalRepresentation = {
    this.copy( links = Links.fromPrototype( this.links ).withLinks( links ).build() )
  }

  /**
    * Adds embedded items for a link-relation type to the HalRepresentation.
    * <p>
    *     If {@code rel} is already present, it is replaced by the new embedded items.
    * </p>
    *
    * @param rel the link-relation type of the embedded items that are added or replaced
    * @param embeddedItems the new values for the specified link-relation type
    * @return this
    * @since 0.1.0
    */
  def withEmbedded( rel: String, embeddedItems: Seq[HalRepresentation] ): HalRepresentation = {
    val b = Embedded.Builder.fromPrototype( this.embedded )
    this.copy( embedded = b.withRepresentations( rel, embeddedItems ).using( this.curies ).build() )
  }

  /**
    * Adds an embedded item for a link-relation type to the HalRepresentation.
    * <p>
    *   The embedded item will be rendered as a single resource object.
    * </p>
    * <p>
    *   If {@code rel} is already present, it is replaced by the new embedded items.
    * </p>
    *
    * @param rel the link-relation type of the embedded item that is added or replaced
    * @param embeddedItem the new value for the specified link-relation type
    * @return this
    *
    * @since 0.1.0
    */
  def withEmbedded( rel: String, embeddedItem: HalRepresentation ): HalRepresentation = {
    val b = Embedded.Builder.fromPrototype( this.embedded )
    this.copy( embedded = b.withRepresentation( rel, embeddedItem ).using( this.curies ).build() )
  }

  /**
    * Merges the Curies of an embedded resource with the Curies of this resource and updates
    * link-relation types in _links and _embedded items.
    *
    * @param curies the Curies of the embedding resource
    * @return this
    */
  def mergeWithEmbedding( curies: Curies ): HalRepresentation = {
    val acc = removeDuplicateCuriesFromEmbedding( this.curies mergeWith curies )

    HalRepresentation(
      links = acc.links.using( acc.curies ),
      embedded = acc.embedded.using( acc.curies ),
      curies = curies
    )
  }

  private def removeDuplicateCuriesFromEmbedding( curies: Curies ): HalRepresentation = {
    val lhs = this

    def isDuplicate( lhsCuri: Link ): Boolean = curies.curies exists { lhsCuri.isEquivalentTo }

    if (lhs.links hasLink Curies.Rel) {
      val lhsCuriLinks: Seq[Link] = lhs.links.linksBy( Curies.Rel ).filter { isDuplicate }
      this.copy(
        links = Links.fromPrototype( this.links ).replace( Curies.Rel, lhsCuriLinks ).build()
      )
    } else {
      this
    }
  }
}

object HalRepresentation {

  /**
    * <p>
    *   Creates a HalRepresentation with {@link Links}, {@link Embedded} objects and a Curies used
    *   to resolve curies from parent representations.
    * </p>
    * <p>
    *   If the Links do contain CURIs, the matching link-relation types of links and embedded
    *   objects are shortened.
    * </p>
    *
    * @param links the Links of the HalRepresentation
    * @param embedded the Embedded items of the HalRepresentation
    * @param curies the Curies used to resolve curies
    * @since 0.1.0
    */
  def apply(
    links: Links = Links.empty,
    embedded: Embedded = Embedded.empty,
    curies: Curies = Curies.empty
  ): HalRepresentation = {
    HalRepresentation(
      links = links.using( curies ),
      embedded = embedded.using( curies ),
      curies = curies,
      attributes = Map.empty[String, String]
    )
  }

  implicit val jsonEncoder: Encoder[HalRepresentation] = new Encoder[HalRepresentation] {
    override def apply( hal: HalRepresentation ): Json = {
      val fields = {
        hal.attributes.mapValues { Json.fromString }.toSeq ++ Seq(
          ( "_links", hal.links.asJson ),
          ( "_embedded", hal.embedded.asJson )
        )
      }

      Json.obj( fields: _* )
    }
  }

  implicit val jsonDecoder: Decoder[HalRepresentation] = new Decoder[HalRepresentation] {
    override def apply( c: HCursor ): Result[HalRepresentation] = {
      val attributes: Seq[( String, String )] = {
        c.keys
          .getOrElse( Iterable.empty[String] )
          .filterNot { key =>
            key != "_links" || key != "_embedded"
          }
          .map { key =>
            c.downField( key )
              .focus
              .map { v =>
                ( key, v.noSpaces )
              }
          }
          .flatten
          .toSeq
      }

      for {
        links    <- c.downField( "_links" ).as[Links]
        embedded <- c.downField( "_embedded" ).as[Embedded]
      } yield {
        HalRepresentation(
          links = links,
          embedded = embedded,
          attributes = Map( attributes: _* ),
          curies = links.curies
        )
      }
    }
  }

  implicit val halEncoder: HalEncoder[HalRepresentation] = {
    HalEncoder.pure[HalRepresentation] { identity }
  }

  implicit val halDecoder: HalDecoder[HalRepresentation] = {
    import cats.syntax.either._
    HalDecoder.pure[HalRepresentation] { rep =>
      rep.asRight
    }
  }
}
