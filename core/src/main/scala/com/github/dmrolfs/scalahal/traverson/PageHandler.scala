package com.github.dmrolfs.scalahal.traverson

import scala.concurrent.Future

/**
  * Functional interface used to traverse pages of linked or embedded resources.
  * <p>
  *   The different {@link Traverson#paginateAs(String, Class, EmbeddedTypeInfo, PageHandler)
  *   pagination} methods of the {@link Traverson} make use of PageHandlers to traverse a single
  *   page.
  * </p>
  * <pre><code>
  *   Traverson.traverson(this::fetchJson)
  *     .startWith("http://example.com/example/collection")
  *     .paginateNext( (Traverson pageTraverson) -&gt; {
  *       pageTraverson
  *         .follow("item")
  *         .streamAs(OtherExtendedHalRepresentation.class)
  *         .forEach(x -&gt; values.add(x.someOtherProperty));
  *       return true;
  *     });
  * </code></pre>
  */
trait PageHandler extends (Traverson => Future[Boolean] )
