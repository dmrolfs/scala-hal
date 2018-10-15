package com.github.dmrolfs.scalahal.hal

import io.circe.Decoder.Result
import io.circe.syntax._
import io.circe.{ Decoder, Encoder, HCursor, Json }

/**
  * <p>
  *   The embedded items of a HalResource.
  * </p>
  * <pre><code>
  * {
  *    "_links": {
  *      ...
  *    },
  *    "_embedded": {
  *      "item" : [
  *        {"description" : "first embedded item (resource object)"},
  *        {"description" : "second embedded item (resource object"}
  *      ],
  *      "example" : {
  *        "description" : "A single resource object"
  *      }
  *    },
  *    "someAttribute" : "Foo"
  * }
  * </code></pre>
  *
  * @see <a href="https://tools.ietf.org/html/draft-kelly-json-hal-08#section-4.1.2">draft-kelly-json-hal-08#section-4.1.2</a>
  * @since 0.1.0
  */
case class Embedded(
  /**
    * The embedded items, mapped by link-relation type.
    *
    * <p>
    *   The values are either List&lt;HalRepresentation&gt; or single HalRepresentation instances.
    * </p>
    */
  items: Map[String, SingleOrArray[HalRepresentation]],
  /**
    * The Curies instance used to resolve curies
    */
  curies: Curies
) {

  /**
    * Checks the existence of embedded items with link-relation type {@code rel}
    *
    * @param rel the link-relation type
    * @return true, if item(s) exists, false otherwise
    *
    * @since 0.1.0
    */
  def hasItem( rel: String ): Boolean = items contains curies.resolve( rel )

  /**
    * Returns true if there is at least one embedded item with link-relation type {@code rel}, and
    * if the item will be rendered as an array of embedded items instead of a single object.
    *
    * @param rel the link-relation type
    * @return boolean
    *
    * @since 0.1.0
    */
  def isArray( rel: String ): Boolean = {
    val resolvedRel = curies.resolve( rel )
    hasItem( rel ) && SingleOrArray.isArray( items( resolvedRel ) )
  }

  protected[scalahal] def using( curies: Curies ): Embedded = {
    Embedded.createWithRepresentations( this.items, curies )
  }

  /**
    * Returns all link-relation types of the embedded items.
    *
    * @return list of link-relation types
    * @since 0.1.0
    */
  def rels: Set[String] = items.keySet

  /**
    * Returns the embedded items by link-relation type.
    * <p>
    * If no items with this type are embedded, an empty list is returned.
    *
    * @param rel the link-relation type
    * @return list of embedded HAL representations for the link-relation type
    *
    * @since 0.1.0
    */
  def itemsBy( rel: String ): Seq[HalRepresentation] = {
    items
      .get( curies.resolve( rel ) )
      .map { _.fold( Seq( _ ), identity ) }
      .getOrElse { Seq.empty[HalRepresentation] }
  }

  /**
    * Returns the embedded items by link-relation type.
    * <p>
    *   This method can be used if the Java type of the embedded representations is known, for
    *   example because the {@link HalParser} is used to map the items to a specific
    *   HalRepresentation:
    * </p>
    * <pre><code>
    * final String json = ...
    *
    * final FooHalRepresentation foo = HalParser
    *         .parse(json)
    *         .as(FooHalRepresentation.class, with("bar", BarHalRepresentation.class));
    *
    * final List&lt;BarHalRepresentation embeddedBars = foo
    *         .getEmbedded()
    *         .getItemsBy("bar", BarHalRepresentation.class);
    * </code></pre>
    *
    * @param rel the link-relation type
    * @param asType the expected class of the embedded items.
    * @param <E> the specific type of the embedded HalRepresentations
    * @return List of E
    * @throws ClassCastException if the expected type does not fit the actual type of the embedded
    *                            items.
    *
    * @since 0.1.0
    */
  def itemsBy[E <: HalRepresentation]( rel: String, asType: Class[E] ): Seq[E] = {
    itemsBy( rel ).map { asType.cast }
  }

  /**
    * @return true if there are no embedded items, false otherwise.
    *
    * @since 0.1.0
    */
  def isEmpty: Boolean = items.isEmpty

  override def equals( rhs: Any ): Boolean = rhs match {
    case that: Embedded => {
      if (this eq that) true
      else {
        (that.## == this.##) &&
        (this.items == that.items)
      }
    }

    case _ => false
  }

  override def hashCode: Int = 41 * (41 + items.##)

  /**
    * {@inheritDoc}
    *
    * @since 0.1.0
    */
  override def toString: String = s"Embedded(items=${items})"

}

object Embedded {

  //  @SuppressWarnings("unchecked")
  //  private Embedded(final Map<String, Object> items, final Curies curies) {
  //    final Map<String, Object> curiedItems = new LinkedHashMap<>();
  //    for (final String rel : items.keySet()) {
  //      final Object itemOrListOfItems = items.get(rel);
  //      if (itemOrListOfItems instanceof List) {
  //        curiedItems.put(curies.resolve(rel), ((List<HalRepresentation>) itemOrListOfItems)
  //          .stream()
  //          .map(halRepresentation -> halRepresentation.mergeWithEmbedding(curies))
  //          .collect(toList()));
  //      } else {
  //        curiedItems.put(curies.resolve(rel), ((HalRepresentation) itemOrListOfItems).mergeWithEmbedding(curies));
  //      }
  //    }
  //    this.items = curiedItems;
  //    this.curies = curies;
  //  }

  def createWithRepresentations(
    items: Map[String, SingleOrArray[HalRepresentation]] = Map(),
    curies: Curies = Curies.empty
  ): Embedded = {
    val curiedItems: Map[String, SingleOrArray[HalRepresentation]] = {
      for {
        relRepresentations <- items
        ( rel, reps ) = relRepresentations
      } yield {
        val curiedReps = reps match {
          case Left( r )   => Left( r mergeWithEmbedding curies )
          case Right( rs ) => Right( rs map { _ mergeWithEmbedding curies } )
        }

        ( curies resolve rel, curiedReps )
      }
    }

    Embedded( items = curiedItems, curies = curies )
  }

  def createWithRepresentation(
    rel: String,
    item: HalRepresentation,
    curies: Curies = Curies.empty
  ): Embedded = {
    createWithRepresentations(
      items = Map( curies.resolve( rel ) -> Left( item.mergeWithEmbedding( curies ) ) ),
      curies = curies
    )
  }

  /**
    * Create an empty embedded instance, without embedded items.
    *
    * @return empty Embedded
    *
    * @since 0.1.0
    */
  val empty: Embedded = Embedded(
    items = Map.empty[String, SingleOrArray[HalRepresentation]],
    curies = Curies.empty
  )

  /**
    * Create an Embedded instance with a single embedded HalRepresentations that will be rendered as
    * a single item instead of an array of embedded items.
    *
    * @param rel the link-relation type of the embedded items
    * @param embeddedItem the single embedded item
    * @return Embedded
    *
    * @since 0.1.0
    */
  def embedded( rel: String, embeddedItem: HalRepresentation ): Embedded = {
    createWithRepresentation( rel, embeddedItem )
  }

  /**
    * Create an Embedded instance with a list of nested HalRepresentations for a single
    * link-relation type.
    *
    * @param rel the link-relation type of the embedded representations
    * @param embeddedRepresentations the list of embedded representations
    * @return Embedded
    *
    * @since 0.1.0
    */
  def embedded( rel: String, embeddedRepresentations: Seq[HalRepresentation] ): Embedded = {
    createWithRepresentations( items = Map( rel -> Right( embeddedRepresentations ) ) )
  }

  /**
    * Create a linksBuilder used to build Embedded instances with more than one link-relation type.
    *
    * @return EmbeddedBuilder
    *
    * @since 0.1.0
    */
  def embeddedBuilder(): Builder = Builder()

  /**
    * <p>
    *   Creates an EmbeddedBuilder initialized from a copy of an Embedded instance.
    * </p>
    * <p>
    *   This is used to add / replace lists of HAL representations for a link-relation type.
    * </p>
    * @param embedded the Embedded instance to be copied.
    * @return EmbeddedBuilder that is initialized using {@code embedded}.
    *
    * @since 0.1.0
    */
  def fromPrototype( prototype: Embedded ): Builder = Builder( embedded = prototype.items )

  final case class Builder private (
    embedded: Map[String, SingleOrArray[HalRepresentation]] = Map(),
    curies: Curies = Curies.empty
  ) {

    /**
      * Adds / replaces the embedded representations for a link-relation type.
      * <p>
      *   The HalRepresentations added using this method will be rendered as an array of object
      *   instead of a {@link #with(String, HalRepresentation) single object}:
      * </p>
      * <pre><code>
      *   {
      *     "_embedded" : {
      *       "foo" : [
      *         {
      *           "_links" : {
      *             "self" : { "href" : "http://example.com/a-single-embedded-foo-item}
      *           }
      *         }
      *       ]
      *     }
      *   }
      * </code></pre>
      *
      * @param rel the link-relation type
      * @param embeddedRepresentations the embedded items
      * @return EmbeddedBuilder
      *
      * @since 0.1.0
      */
    def withRepresentations(
      rel: String,
      embeddedRepresentations: Seq[HalRepresentation]
    ): Builder = {
      this.copy( embedded = this.embedded + (rel -> Right( embeddedRepresentations )) )
    }

    /**
      * Adds / replaces the embedded representation for a link-relation type.
      * <p>
      *   The single HalRepresentation added using this method will be rendered as a single object
      *   instead of an {@link #with(String, List) array of objects}:
      * </p>
      * <pre><code>
      *   {
      *     "_embedded" : {
      *       "foo" : {
      *         "_links" : {
      *           "self" : { "href" : "http://example.com/a-single-embedded-foo-item"}
      *         }
      *       }
      *     }
      *   }
      * </code></pre>
      *
      * @param rel the link-relation type
      * @param embeddedRepresentation the single embedded item
      * @return EmbeddedBuilder
      *
      * @since 0.1.0
      */
    def withRepresentation(
      rel: String,
      embeddedRepresentation: HalRepresentation
    ): Builder = {
      this.copy( embedded = this.embedded + (rel -> Left( embeddedRepresentation )) )
    }

    /**
      * Removes the embedded representations for a link-relation type.
      *
      * @param rel the link-relation type
      * @return EmbeddedBuilder
      *
      * @since 0.1.0
      */
    def without( rel: String ): Builder = this.copy( embedded = this.embedded - rel )

    protected[scalahal] def using( curies: Curies ): Builder = this.copy( curies = curies )

    /**
      * Builds an Embedded instance.
      *
      * @return Embedded
      *
      * @since 0.1.0
      */
    def build(): Embedded = {
      if (embedded.isEmpty) empty else createWithRepresentations( embedded, curies )
    }
  }

  implicit val encoder: Encoder[Embedded] = new Encoder[Embedded] {
    override def apply( e: Embedded ): Json = {
      val fields: Seq[( String, Json )] = {
        e.items.toSeq
          .map { case ( k, v ) => ( k, v.fold( _.asJson, _.asJson ) ) }
      }

      Json.obj( fields: _* )
    }
  }

  implicit val decoder: Decoder[Embedded] = new Decoder[Embedded] {
    import cats.instances.either._
    import cats.instances.list._
    import cats.syntax.traverse._

    override def apply( c: HCursor ): Result[Embedded] = {
      val parsedReps: Iterable[Decoder.Result[RelPair[HalRepresentation]]] = {
        for {
          ks <- c.keys.toIterable
          k  <- ks
        } yield {
          c.downField( k )
            .focus
            .map {
              case json if json.isArray => json.as[Seq[HalRepresentation]] map { Right( _ ) }
              case json                 => json.as[HalRepresentation] map { Left( _ ) }
            }
            .map { values =>
              values map { ( k, _ ) }
            }
            .getOrElse {
              Right( ( k, Right( Seq.empty[HalRepresentation] ) ) )
            }
        }
      }

      parsedReps.toList.sequence map { relRep =>
        val all: Map[String, SingleOrArray[HalRepresentation]] = Map( relRep: _* )
//        val curies = {
//          val r1: Int = all
//            .get( Curies.Rel )
//            .map { _.fold( Seq(_), identity ) }
//            r1.map { Curies.apply }
//            .getOrElse { Curies.empty }
//        }
        createWithRepresentations( all )
      }
    }
  }
//  /**
//    * The Jackson JsonSerializer used to serialize Embedded instances to JSON.
//    *
//    * @since 0.1.0
//    */
//  public static class EmbeddedSerializer extends JsonSerializer<Embedded> {
//
//    @Override
//    public void serialize(Embedded value, JsonGenerator gen, SerializerProvider serializers) throws IOException {
//      gen.writeObject(value.items);
//    }
//  }
//
//  /**
//    * The Jackson JsonDeserializer used to deserialize JSON into Embedded instances.
//    *
//    * @since 0.1.0
//    */
//  public static class EmbeddedDeserializer extends JsonDeserializer<Embedded> {
//
//    private static final TypeReference<Map<String, List<HalRepresentation>>> TYPE_REF_LIST_OF_HAL_REPRESENTATIONS = new TypeReference<Map<String, List<HalRepresentation>>>() {};
//
//    @Override
//    public Embedded deserialize(JsonParser p, DeserializationContext ctxt) throws IOException {
//      try {
//        final Map<String, Object> items = p.readValueAs(TYPE_REF_LIST_OF_HAL_REPRESENTATIONS);
//        return new Embedded(items);
//      } catch (final JsonMappingException e) {
//        if (e.getMessage().contains("Can not deserialize instance of java.util.ArrayList out of START_OBJECT token")) {
//          throw new JsonMappingException(p, "Can not deserialize single embedded items for a link-relation type. Try using the HalParser, or configure your ObjectMapper: 'objectMapper.configure(DeserializationFeature.ACCEPT_SINGLE_VALUE_AS_ARRAY, true)'.", e);
//        } else {
//          throw e;
//        }
//      }
//    }
//  }

}
