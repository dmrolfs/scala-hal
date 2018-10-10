//package com.github.dmrolfs.scalahal
//
///**
//  * <p>
//  *   A parser used to parse application/hal+json representations of REST resources into Java
//  *   objects.
//  * </p>
//  * <p>
//  *   Simple HAL representations can be parsed using Circe like this:
//  * </p>
//  * <pre><code>
////  *     new ObjectMapper().readValue(json.getBytes(), MyHalRepresentation.class)
//  * </code></pre>
//  * <p>
//  *     The same can be achieved by using a HalParser:
//  * </p>
//  * <pre><code>
//  *     final MyHalRepresentation result = HalParser.parse(json).as(MyHalRepresentation.class);
//  * </code></pre>
//  * <p>
//  *   However, if the representation contains embedded items, Circe is unable to determine the type
//  *   of the embedded items, because the HAL document itself does not contain type information. In
//  *   this case, Circe needs some help to identify the concrete types of embedded items. Using the
//  *   HalParser, this is accomplished like this:
//  * </p>
//  * <pre><code>
//  *     final FooHalRepresentation result = HalParser
//  *             .parse(json)
//  *             .as[Hal[Foo]](
//  *               withEmbedded[Hal[Bar]]("bar")
//  *             )
//  * </code></pre>
//  *
//  * @since 0.1.0
//  */
//class HalParser private ( json: String ) {}
//
//object HalParser {
//
//  /**
//    * Create a new HalParser for a JSON document.
//    * <p>
//    *   The {@link #DEFAULT_JSON_MAPPER} is used to parse documents.
//    * </p>
//    * @param json the application/hal+json document to be parsed.
//    * @return HalParser instance.
//    *
//    * @since 0.1.0
//    */
//  def parse( json: String ): HalParser = new HalParser( json )
//
//}
