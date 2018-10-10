package com.github.dmrolfs.scalahal.traverson

import java.io.IOException
import java.net.URL
import scala.concurrent.{ ExecutionContext, Future }
import scala.collection.JavaConverters._
import scala.reflect.{ classTag, ClassTag }
import cats.data.NonEmptyList
import io.circe._
//import io.circe.syntax._
import org.slf4j.LoggerFactory
import cats.syntax.either._
import com.damnhandy.uri.template.UriTemplate
import com.github.dmrolfs.scalahal.{ EC, ErrorOr, HalDecoder }
import com.github.dmrolfs.scalahal.hal.{ EmbeddedTypeInfo, HalRepresentation, Link }
import com.github.dmrolfs.scalahal.traverson.Traverson.{ log, Hop }

/**
  * A Traverson is a utility that makes it easy to navigate REST APIs using HAL+JSON. It is
  * motivated by the JS Traverson library.
  * <p>
  *   {@link #startWith(String) Starting with} a href to an initial resource, you can
  *   {@link #follow(String) follow} one or more links identified by their link-relation type.
  * </p>
  * <p>
  *   {@link Link#isTemplated()}  Templated links} can be expanded using template
  *   {@link #withVars(String, Object, Object...) variables}.
  * </p>
  * <p>
  *   Paginated resources can be processed using the different {@link #paginateNext(PageHandler)},
  *   {@link #paginateNextAs(Class, PageHandler)}, etc. methods.
  * </p>
  * <p>
  *   Example:
  * </p>
  * <pre><code>
  *   final Optional&lt;HalRepresentation&gt; hal = traverson(this::myHttpGetFunction)
  *     .startWith("http://example.org")
  *     .follow("foobar")
  *     .follow(
  *       hops("foo", "bar"),
  *       withVars("param1", "value1", "param2", "value2"))
  *     .getResource();
  * </code></pre>
  * <p>
  *   Embedded resources are used instead of linked resources, if present.
  * </p>
  */
case class Traverson private (
  linkResolver: LinkResolver,
  private val _startWith: Option[URL] = None,
  /**
    * The current contextUrl of the Traverson.
    * <p>
    *   The contextUrl is used to resolve relative links / HREFs to other resources.
    * </p>
    */
  contextUrl: Option[URL] = None,
  hops: Seq[Hop] = Seq.empty[Hop],
  accumulation: Seq[HalRepresentation] = Seq.empty[HalRepresentation]
) {

  /**
    * Start traversal at the application/hal+json resource idenfied by {@code uri}.
    *
    * @param uriTemplate the {@code URI} of the initial HAL resource.
    * @param vars uri-template variables used to build links.
    * @return Traverson initialized with the {@link HalRepresentation} identified by {@code uri}.
    */
  def startWith( uriTemplate: String, vars: Map[String, AnyRef] ): Traverson = {
    startWith( UriTemplate.fromTemplate( uriTemplate ).expand( vars.asJava ) )
  }

  /**
    * Start traversal at the application/hal+json resource identified by {@code uri}.
    *
    * @param uri the {@code URI} of the initial HAL resource.
    * @return Traverson initialized with the {@link HalRepresentation} identified by {@code uri}.
    */
  def startWith( uri: String ): Traverson = {
    val start = Option( Traverson.hrefToUrl( uri ).valueOr { throw _ } )
    this.copy( _startWith = start, contextUrl = start )
  }

  /**
    * Start traversal at the given HAL resource.
    *
    * <p>
    *   It is expected, that the HalRepresentation has an absolute 'self' link, or that all links to
    *   other resources are absolute links. If this is not assured,
    *   {@link #startWith(URL, HalRepresentation)} must be used, so relative links can be resolved.
    * </p>
    *
    * @param resource the initial HAL resource.
    * @return Traverson initialized with the specified {@link HalRepresentation}.
    */

  def startWith( resource: HalRepresentation ): Traverson = {
    val newAcc = accumulation :+ resource
    val self = resource.links.linkBy( "self" )

    val ctx = {
      self
        .map { Traverson.linkToUrl }
        .map { _.valueOr { throw _ } }
    }

    if (ctx.isEmpty) {
      val hasRelativeLink = {
        resource.links.links.values.flatten.exists { _.href.matches( """http.*//.*""" ) }
      }

      if (hasRelativeLink) {
        throw new IllegalArgumentException(
          "Unable to construct Traverson from HalRepresentation w/o self link but containing " +
          "relative links. Please try Traverson.startWith(URL, HalRepresentation)"
        )
      }
    }

    this.copy(
      _startWith = None,
      contextUrl = ctx,
      accumulation = newAcc
    )
  }

  /**
    * Start traversal at the given HAL resource, using the {@code contextUrl} to resolve relative
    * links.
    *
    * <p>
    *   If the {@code resource} is obtained from another Traverson,
    *   {@link Traverson#getCurrentContextUrl()} can be called to get this URL.
    * </p>
    *
    * @param contextUrl URL of the Traverson's current context, used to resolve relative links
    * @param resource the initial HAL resource.
    * @return Traverson initialized with the specified {@link HalRepresentation} and {@code contextUrl}.
    * @since 1.0.0
    */
  def startWith( contextUrl: URL, resource: HalRepresentation ): Traverson = {
    this.copy(
      _startWith = None,
      contextUrl = Some( contextUrl ),
      accumulation = Seq( resource )
    )
  }

  /**
    * Follow multiple link-relation types, one by one. The {@link LinkPredicates predicate} is used
    * to select the matching links, if there are more than one per link-relation type.
    * <p>
    *   Templated links are expanded to URIs using the specified template variables.
    * </p>
    * <p>
    *   Embedded items are used instead of resolving links, if present in the returned HAL
    *   documents.
    * </p>
    *
    * @param rels the link-relation types of the followed links
    * @param predicate the predicate used to select the link to follow
    * @param vars uri-template variables used to build links.
    * @return this
    */
  def follow(
    rels: String*
  )(
    shouldFollow: Hop.FollowLink = Hop.always,
    vars: Map[String, AnyRef] = Map.empty[String, AnyRef]
  ): Traverson = {
    def loop( rel: String, acc: Traverson ): Traverson = {
      checkState()
      acc.copy( hops = acc.hops :+ Hop( rel, shouldFollow, vars, ignoreEmbedded = false ) )
    }

    rels.foldLeft( this ) { ( acc, rel ) =>
      loop( rel, acc )
    }
  }

  /**
    * Follow the first {@link Link} of the current resource, selected by its link-relation type. The
    * {@link LinkPredicates predicate} is used to select the matching link, if multiple links are
    * available for the specified link-relation type.
    * <p>
    *   Templated links are expanded to URIs using the specified template variables.
    * </p>
    * <p>
    *   Other than {@link #follow(String, Predicate, Map)}, this method will ignore embedded
    *   resources, if a link with matching link-relation type is present, Only if the link is
    *   missing, an optional embedded resource is used.
    * </p>
    * @param rel the link-relation type of the followed link
    * @param predicate the predicate used to select the link to follow.
    * @param vars uri-template variables used to build links.
    * @return this
    */
  def followLink(
    rel: String,
    shouldFollow: Hop.FollowLink = Hop.always,
    vars: Map[String, AnyRef] = Map.empty[String, AnyRef]
  ): Traverson = {
    checkState()
    this.copy( hops = this.hops :+ Hop( rel, shouldFollow, vars, ignoreEmbedded = true ) )
  }

  /**
    * Iterates over pages by following 'next' links. For every page, a {@code Traverson} is created
    * and provided as a parameter to the callback function.
    *
    * <pre>
    *                              next
    *     ... --&gt; HalRepresentation --&gt; HalRepresentation
    *                     |                     |
    *                     v N item              v N item
    *              HalRepresentation     HalRepresentation
    * </pre>
    *
    * <p>
    *   The {@code Traverson} is backed by a {@link HalRepresentation} with no type information. If
    *   pages may contain embedded items, and if a specific sub-type of HalRepresentation is
    *   required for items, {@link #paginateNext(EmbeddedTypeInfo,PageHandler)} or
    *   {@link #paginateNextAs(Class,EmbeddedTypeInfo,PageHandler)} should be used instead of this
    *   method.
    * </p>
    *
    * <p>
    *   Iteration stops if the callback returns {@code false}, or if the last page is processed.
    * </p>
    *
    * @param pageHandler the callback used to process pages of items.
    * @throws IOException if a low-level I/O problem (unexpected end-of-input, network error) occurs.
    * @throws JsonParseException if the json document can not be parsed by Jackson's ObjectMapper
    * @throws JsonMappingException if the input JSON structure can not be mapped to the specified HalRepresentation type
    */
  def paginateNext( handler: PageHandler )( implicit ec: ExecutionContext ): Future[Done] = {
    paginate( "next", handler )
  }
//  public void paginateNext(final PageHandler pageHandler) throws IOException {
//    paginateAs("next", HalRepresentation.class, null, pageHandler);
//  }

  /**
    * Iterates over pages by following 'next' links. For every page, a {@code Traverson} is created and provided as a
    * parameter to the callback function.
    *
    * <pre>
    *                              next
    *     ... --&gt; HalRepresentation --&gt; HalRepresentation
    *                     |                     |
    *                     v N item              v N item
    *               &lt;embedded type&gt;      &lt;embedded type&gt;
    * </pre>
    *
    * <p>
    *     The {@code Traverson} is backed by a {@link HalRepresentation} with {@link EmbeddedTypeInfo}. This way it
    *     is possible to access items embedded into the page resources as specific subtypes of HalRepresentation.
    * </p>
    *
    * <p>
    *     Iteration stops if the callback returns {@code false}, or if the last page is processed.
    * </p>
    *
    * @param embeddedTypeInfo type information about possibly embedded items.
    * @param pageHandler the callback used to process pages of items.
    * @throws IOException if a low-level I/O problem (unexpected end-of-input, network error) occurs.
    * @throws JsonParseException if the json document can not be parsed by Jackson's ObjectMapper
    * @throws JsonMappingException if the input JSON structure can not be mapped to the specified HalRepresentation type
    * @since 1.0.0
    */
  //not needed
//  public void paginateNext(final EmbeddedTypeInfo embeddedTypeInfo,
//  final PageHandler pageHandler) throws IOException {
//    paginateAs("next", HalRepresentation.class, embeddedTypeInfo, pageHandler);
//  }

  /**
    * Iterates over pages by following 'next' links. For every page, a {@code Traverson} is created and provided as a
    * parameter to the callback function.
    *
    * <pre>
    *                        next
    *     ... --&gt; &lt;page type&gt; --&gt; &lt;page type&gt;
    *                 |                 |
    *                 v N item          v N item
    *         HalRepresentation  HalRepresentation
    * </pre>
    *
    * <p>
    *     The {@code Traverson} is backed by a representation of {@code pageType}, but without
    *     {@link EmbeddedTypeInfo}. This way it
    *     is possible to access special attributes of the page by calling {@code pageTraverson.getResourceAs(pageType)}
    *     in the callback function. If the paged items need to be accessed as a subtype of HalRepresentation, you
    *     can call pageTraverson.follow(rel).streamAs(MyItemType.class) - but ONLY if the items are not embedded into
    *     the page.
    * </p>
    *
    * <p>
    *     For embedded items having a subtype of HalRepresentation, {@link #paginateNextAs(Class, EmbeddedTypeInfo, PageHandler)}
    *     must be used instead of this method, otherwise a {@code ClassCastException} will be thrown.
    * </p>
    *
    * <p>
    *     Iteration stops if the callback returns {@code false}, or if the last page is processed.
    * </p>
    *
    * @param pageType the subtype of HalRepresentation of the page resources
    * @param pageHandler callback function called for every page
    * @param <T> subtype of HalRepresentation
    * @throws IOException if a low-level I/O problem (unexpected end-of-input, network error) occurs.
    * @throws JsonParseException if the json document can not be parsed by Jackson's ObjectMapper
    * @throws JsonMappingException if the input JSON structure can not be mapped to the specified HalRepresentation type
    * @since 1.0.0
    */
  //not needed
//  public <T extends HalRepresentation> void paginateNextAs(final Class<T> pageType,
//  final PageHandler pageHandler) throws IOException {
//    paginateAs("next", pageType, null, pageHandler);
//  }

  /**
    * Iterates over pages by following 'next' links. For every page, a {@code Traverson} is created and provided as a
    * parameter to the callback function.
    *
    * <pre>
    *                        next
    *     ... --&gt; &lt;page type&gt; --&gt; &lt;page type&gt;
    *                  |               |
    *                  v N item        v N item
    *           &lt;embedded type&gt;  &lt;embedded type&gt;
    * </pre>
    *
    * <p>
    *     The {@code Traverson} is backed by a representation of {@code pageType} with {@link EmbeddedTypeInfo}.
    *     This way it is possible to access items embedded into the page resources as specific subtypes of
    *     HalRepresentation.
    * </p>
    *
    * <p>
    *     Iteration stops if the callback returns {@code false}, or if the last page is processed.
    * </p>
    *
    *
    * @param pageType the subtype of HalRepresentation of the page resources
    * @param embeddedTypeInfo type information of the (possibly embedded) items of a page
    * @param pageHandler callback function called for every page
    * @param <T> subtype of HalRepresentation
    * @throws IOException if a low-level I/O problem (unexpected end-of-input, network error) occurs.
    * @throws JsonParseException if the json document can not be parsed by Jackson's ObjectMapper
    * @throws JsonMappingException if the input JSON structure can not be mapped to the specified HalRepresentation type
    * @since 1.0.0
    */
  //not needed
//  public <T extends HalRepresentation> void paginateNextAs(final Class<T> pageType,
//  final EmbeddedTypeInfo embeddedTypeInfo,
//  final PageHandler pageHandler) throws IOException {
//    paginateAs("next", pageType, embeddedTypeInfo, pageHandler);
//  }

  /**
    * Iterates over pages by following 'prev' links. For every page, a {@code Traverson} is created and provided as a
    * parameter to the callback function.
    *
    * <pre>
    *                              prev
    *     ... --&gt; HalRepresentation --&gt; HalRepresentation
    *                     |                     |
    *                     v N item              v N item
    *              HalRepresentation     HalRepresentation
    * </pre>
    *
    * <p>
    *     The {@code Traverson} is backed by a {@link HalRepresentation} with no type information. If pages may contain
    *     embedded items, and if a specific sub-type of HalRepresentation is required for items,
    *     {@link #paginateNext(EmbeddedTypeInfo,PageHandler)} or {@link #paginateNextAs(Class,EmbeddedTypeInfo,PageHandler)}
    *     should be used instead of this method.
    * </p>
    *
    * <p>
    *     Iteration stops if the callback returns {@code false}, or if the last page is processed.
    * </p>
    *
    * @param pageHandler the callback used to process pages of items.
    * @throws IOException if a low-level I/O problem (unexpected end-of-input, network error) occurs.
    * @throws JsonParseException if the json document can not be parsed by Jackson's ObjectMapper
    * @throws JsonMappingException if the input JSON structure can not be mapped to the specified HalRepresentation type
    * @since 1.0.0
    */
  def paginatePrev( handler: PageHandler )( implicit ec: ExecutionContext ): Future[Done] = {
    paginate( "prev", handler )
  }

  /**
    * Iterates over pages by following 'prev' links. For every page, a {@code Traverson} is created and provided as a
    * parameter to the callback function.
    *
    * <pre>
    *                              prev
    *     ... --&gt; HalRepresentation --&gt; HalRepresentation
    *                     |                     |
    *                     v N item              v N item
    *               &lt;embedded type&gt;      &lt;embedded type&gt;
    * </pre>
    *
    * <p>
    *     The {@code Traverson} is backed by a {@link HalRepresentation} with {@link EmbeddedTypeInfo}. This way it
    *     is possible to access items embedded into the page resources as specific subtypes of HalRepresentation.
    * </p>
    *
    * <p>
    *     Iteration stops if the callback returns {@code false}, or if the last page is processed.
    * </p>
    *
    * @param embeddedTypeInfo type information of the (possibly embedded) items of a page
    * @param pageHandler callback function called for every page
    * @throws IOException if a low-level I/O problem (unexpected end-of-input, network error) occurs.
    * @throws JsonParseException if the json document can not be parsed by Jackson's ObjectMapper
    * @throws JsonMappingException if the input JSON structure can not be mapped to the specified HalRepresentation type
    * @since 1.0.0
    */
  // not needed
//  public void paginatePrev(final EmbeddedTypeInfo embeddedTypeInfo,
//  final PageHandler pageHandler) throws IOException {
//    paginateAs("prev", HalRepresentation.class, embeddedTypeInfo, pageHandler);
//  }

  /**
    * Iterates over pages by following 'prev' links. For every page, a {@code Traverson} is created and provided as a
    * parameter to the callback function.
    *
    * <pre>
    *                         prev
    *     ... --&gt; &lt;page type&gt; --&gt; &lt;page type&gt;
    *                 |                 |
    *                 v N item          v N item
    *          HalRepresentation   HalRepresentation
    * </pre>
    *
    * <p>
    *     The {@code Traverson} is backed by a representation of {@code pageType}, but without
    *     {@link EmbeddedTypeInfo}. This way it is possible to access special attributes of the page by
    *     calling {@code pageTraverson.getResourceAs(pageType)} in the callback function. If the paged items need to
    *     be accessed as a subtype of HalRepresentation, you can call pageTraverson.follow(rel).streamAs(MyItemType.class)
    *     - but ONLY if the items are not embedded into the page.
    * </p>
    *
    * <p>
    *     For embedded items having a subtype of HalRepresentation, {@link #paginatePrevAs(Class, EmbeddedTypeInfo, PageHandler)}
    *     must be used instead of this method, otherwise a {@code ClassCastException} will be thrown.
    * </p>
    *
    * <p>
    *     Iteration stops if the callback returns {@code false}, or if the last page is processed.
    * </p>
    *
    * @param pageType the subtype of HalRepresentation of the page resources
    * @param pageHandler callback function called for every page
    * @param <T> subtype of HalRepresentation
    * @throws IOException if a low-level I/O problem (unexpected end-of-input, network error) occurs.
    * @throws JsonParseException if the json document can not be parsed by Jackson's ObjectMapper
    * @throws JsonMappingException if the input JSON structure can not be mapped to the specified HalRepresentation type
    * @since 1.0.0
    */
  // not needed
//  public <T extends HalRepresentation> void paginatePrevAs(final Class<T> pageType,
//  final PageHandler pageHandler) throws IOException {
//    paginateAs("prev", pageType, null, pageHandler);
//  }

  /**
    * Iterates over pages by following 'prev' links. For every page, a {@code Traverson} is created and provided as a
    * parameter to the callback function.
    *
    * <pre>
    *                         prev
    *     ... --&gt; &lt;page type&gt; --&gt; &lt;page type&gt;
    *                  |               |
    *                  v N item        v N item
    *           &lt;embedded type&gt;  &lt;embedded type&gt;
    * </pre>
    *
    * <p>
    *     The {@code Traverson} is backed by a representation of {@code pageType} with {@link EmbeddedTypeInfo}.
    *     This way it is possible to access items embedded into the page resources as specific subtypes of
    *     HalRepresentation.
    * </p>
    *
    * <p>
    *     Iteration stops if the callback returns {@code false}, or if the last page is processed.
    * </p>
    *
    *
    * @param pageType the subtype of HalRepresentation of the page resources
    * @param embeddedTypeInfo type information of the (possibly embedded) items of a page
    * @param pageHandler callback function called for every page
    * @param <T> subtype of HalRepresentation
    * @throws IOException if a low-level I/O problem (unexpected end-of-input, network error) occurs.
    * @throws JsonParseException if the json document can not be parsed by Jackson's ObjectMapper
    * @throws JsonMappingException if the input JSON structure can not be mapped to the specified HalRepresentation type
    * @since 1.0.0
    */
  //not needed

  /**
    * Iterates over pages by following {code rel} links. For every page, a {@code Traverson} is created and provided as a
    * parameter to the {@link PageHandler} function.
    *
    * <pre>
    *                         &lt;rel&gt;
    *     ... --&gt; &lt;page type&gt; --&gt; &lt;page type&gt;
    *                  |                |
    *                  v N item         v N item
    *           &lt;embedded type&gt;   &lt;embedded type&gt;
    * </pre>
    *
    * <p>
    *     The {@code Traverson} is backed by a {@link HalRepresentation} with {@link EmbeddedTypeInfo}.
    *     This way it is possible to access items embedded into the page resources as specific subtypes of
    *     HalRepresentation.
    * </p>
    *
    * <p>
    *     Iteration stops if the callback returns {@code false}, or if the last page is processed.
    * </p>
    *
    * @param rel link-relation type of the links used to traverse pages
    * @param embeddedTypeInfo type information of the (possibly embedded) items of a page
    * @param pageHandler callback function called for every page
    * @param <T> subtype of HalRepresentation
    * @throws IOException if a low-level I/O problem (unexpected end-of-input, network error) occurs.
    * @throws JsonParseException if the json document can not be parsed by Jackson's ObjectMapper
    * @throws JsonMappingException if the input JSON structure can not be mapped to the specified HalRepresentation type
    * @since 2.0.0
    */
  //not needed
//  public <T extends HalRepresentation> void paginate(final String rel,
//  final EmbeddedTypeInfo embeddedTypeInfo,
//  final PageHandler pageHandler) throws IOException {
//    paginateAs(rel, HalRepresentation.class, embeddedTypeInfo, pageHandler);
//  }

  /**
    * Iterates over pages by following {code rel} links. For every page, a {@code Traverson} is
    * created and provided as a parameter to the {@link PageHandler} function.
    *
    * <pre>
    *                         &lt;rel&gt;
    *     ... --&gt; &lt;page type&gt; --&gt; &lt;page type&gt;
    *                  |                |
    *                  v N item         v N item
    *           &lt;embedded type&gt;   &lt;embedded type&gt;
    * </pre>
    *
    * <p>
    *   The {@code Traverson} is backed by a representation of {@code pageType} with
    *   {@link EmbeddedTypeInfo}. This way it is possible to access items embedded into the page
    *   resources as specific subtypes of HalRepresentation.
    * </p>
    *
    * <p>
    *   Iteration stops if the callback returns {@code false}, or if the last page is processed.
    * </p>
    *
    * @param rel link-relation type of the links used to traverse pages
    * @param embeddedTypeInfo type information of the (possibly embedded) items of a page
    * @param pageHandler callback function called for every page
    * @param <T> subtype of HalRepresentation
    * @throws IOException if a low-level I/O problem (unexpected end-of-input, network error)
    *                     occurs.
    * @throws JsonParseException if the json document can not be parsed by Jackson's ObjectMapper
    * @throws JsonMappingException if the input JSON structure can not be mapped to the specified
    *                              HalRepresentation type
    */
  def paginate(
    rel: String,
    handler: PageHandler
  )(
    implicit ec: ExecutionContext
  ): Future[Done] = {
    def makeTraverson( currentPage: HalRepresentation, ctx: URL ): Traverson = {
      Traverson.makeWithResolver( this.linkResolver ).startWith( ctx, currentPage )
    }

    def loop( currentPage: HalRepresentation, ctx: URL ): Future[Done] = {
      handler( makeTraverson( currentPage, ctx ) )
        .flatMap {
          case false => Future successful Done

          case true => {
            if (!currentPage.links.rels.contains( rel )) Future successful Done
            else {
              follow( rel )().representation
                .flatMap { nextPage =>
                  nextPage.fold[Future[Done]] {
                    Future successful Done
                  } {
                    loop( _, ctx )
                  }
                }
            }
          }
        }
    }

    this.representation
      .flatMap { currentPage =>
        val done = {
          for {
            current <- currentPage
            ctxurl  <- this.contextUrl
          } yield {
            loop( current, ctxurl )
          }
        }

        done.fold[Future[Done]] {
          Future successful Done
        } { fd =>
          fd
        }
      }
  }
//
//  public <T extends HalRepresentation> void paginateAs(final String rel,
//  final Class<T> pageType,
//  final EmbeddedTypeInfo embeddedTypeInfo,
//  final PageHandler pageHandler) throws IOException {

//    Optional<T> currentPage = getResourceAs(pageType, embeddedTypeInfo);
//    while (currentPage.isPresent()
//      && pageHandler.apply(traverson(linkResolver).startWith(contextUrl, currentPage.get()))
//      && currentPage.get().getLinks().getRels().contains(rel)) {
//      currentPage = follow(rel).getResourceAs(pageType, embeddedTypeInfo);
//    }

//  }

  /**
    * Follow the {@link Link}s of the current resource, selected by its link-relation type and returns a {@link Stream}
    * containing the returned {@link HalRepresentation HalRepresentations}.
    * <p>
    *     If the current node has {@link Embedded embedded} items with the specified {@code rel},
    *     these items are used instead of resolving the associated {@link Link}.
    * </p>
    * <p>
    *     Many times, you do not need the HalRepresentations, but subtypes of HalRepresentation,
    *     so you are able to access custom attributes of your resources. In this case, you need
    *     to use {@link #streamAs(Class)} instead of this method.
    * </p>
    *
    * @return this
    * @throws IOException if a low-level I/O problem (unexpected end-of-input, network error) occurs.
    * @throws JsonParseException if the json document can not be parsed by Jackson's ObjectMapper
    * @throws JsonMappingException if the input JSON structure can not be mapped to the specified HalRepresentation type
    * @since 1.0.0
    */
//  public Stream<HalRepresentation> stream() throws IOException {
//    return streamAs(HalRepresentation.class, null);
//  }

  /**
    * Follow the {@link Link}s of the current resource, selected by its link-relation type and returns a {@link Stream}
    * containing the returned {@link HalRepresentation HalRepresentations}.
    * <p>
    *     Templated links are expanded to URIs using the specified template variables.
    * </p>
    * <p>
    *     If the current node has {@link Embedded embedded} items with the specified {@code rel},
    *     these items are used instead of resolving the associated {@link Link}.
    * </p>
    * @param type the specific type of the returned HalRepresentations
    * @param <T> type of the returned HalRepresentations
    * @return this
    * @throws IOException if a low-level I/O problem (unexpected end-of-input, network error) occurs.
    * @throws JsonParseException if the json document can not be parsed by Jackson's ObjectMapper
    * @throws JsonMappingException if the input JSON structure can not be mapped to the specified HalRepresentation type
    * @since 1.0.0
    */
//  public <T extends HalRepresentation> Stream<T> streamAs(final Class<T> type) throws IOException {
//    return streamAs(type, null);
//  }

  /**
    * Follow the {@link Link}s of the current resource, selected by its link-relation type and returns a {@link Stream}
    * containing the returned {@link HalRepresentation HalRepresentations}.
    * <p>
    *     The EmbeddedTypeInfo is used to define the specific type of embedded items.
    * </p>
    * <p>
    *     Templated links are expanded to URIs using the specified template variables.
    * </p>
    * <p>
    *     If the current node has {@link Embedded embedded} items with the specified {@code rel},
    *     these items are used instead of resolving the associated {@link Link}.
    * </p>
    * @param type the specific type of the returned HalRepresentations
    * @param embeddedTypeInfo specification of the type of embedded items
    * @param moreEmbeddedTypeInfos more embedded type-infos
    * @param <T> type of the returned HalRepresentations
    * @return this
    * @throws IOException if a low-level I/O problem (unexpected end-of-input, network error) occurs.
    * @throws JsonParseException if the json document can not be parsed by Jackson's ObjectMapper
    * @throws JsonMappingException if the input JSON structure can not be mapped to the specified HalRepresentation type
    * @since 1.0.0
    */
//  @SuppressWarnings("unchecked")
//  public <T extends HalRepresentation> Stream<T> streamAs(final Class<T> type,
//  final EmbeddedTypeInfo embeddedTypeInfo,
//  final EmbeddedTypeInfo... moreEmbeddedTypeInfos) throws IOException {
//    if (moreEmbeddedTypeInfos == null || moreEmbeddedTypeInfos.length == 0) {
//      return streamAs(type, embeddedTypeInfo != null ? singletonList(embeddedTypeInfo) : emptyList());
//    } else {
//      final List<EmbeddedTypeInfo> typeInfos = new ArrayList<>();
//      typeInfos.add(requireNonNull(embeddedTypeInfo));
//      typeInfos.addAll(asList(moreEmbeddedTypeInfos));
//      return streamAs(type, typeInfos);
//    }
//  }

  /**
    * Follow the {@link Link}s of the current resource, selected by its link-relation type and returns a {@link Stream}
    * containing the returned {@link HalRepresentation HalRepresentations}.
    * <p>
    *     The EmbeddedTypeInfo is used to define the specific type of embedded items.
    * </p>
    * <p>
    *     Templated links are expanded to URIs using the specified template variables.
    * </p>
    * <p>
    *     If the current node has {@link Embedded embedded} items with the specified {@code rel},
    *     these items are used instead of resolving the associated {@link Link}.
    * </p>
    * @param type the specific type of the returned HalRepresentations
    * @param embeddedTypeInfo specification of the type of embedded items
    * @param <T> type of the returned HalRepresentations
    * @return this
    * @throws IOException if a low-level I/O problem (unexpected end-of-input, network error) occurs.
    * @throws JsonParseException if the json document can not be parsed by Jackson's ObjectMapper
    * @throws JsonMappingException if the input JSON structure can not be mapped to the specified HalRepresentation type
    * @since 1.0.0
    */
//  @SuppressWarnings("unchecked")
//  public <T extends HalRepresentation> Stream<T> streamAs(final Class<T> type,
//  final List<EmbeddedTypeInfo> embeddedTypeInfo) throws IOException {
//    checkState();
//    try {
//      if (startWith != null) {
//        lastResult = traverseInitialResource(type, embeddedTypeInfo, true);
//      } else if (!hops.isEmpty()) {
//        lastResult = traverseHop(lastResult.get(0), type, embeddedTypeInfo, true);
//      }
//      return (Stream<T>) lastResult.stream();
//    } catch (final Exception e) {
//      LOG.error(e.getMessage(), e);
//      throw e;
//    }
//  }

  /**
    * Return the selected resource as HalRepresentation.
    * <p>
    *   If there are multiple matching representations, the first node is returned.
    * </p>
    * <p>
    *   Many times, you do not need the HalRepresentation, but a subtype of HalRepresentation,
    *   so you are able to access custom attributes of your resource. In this case, you need
    *   to use {@link #getResourceAs(Class)} instead of this method.
    * </p>
    *
    * @return HalRepresentation
    * @throws IOException if a low-level I/O problem (unexpected end-of-input, network error) occurs.
    * @throws JsonParseException if the json document can not be parsed by Jackson's ObjectMapper
    * @throws JsonMappingException if the input JSON structure can not be mapped to the specified HalRepresentation type
    * @since 1.0.0
    */
  def representation( implicit ec: ExecutionContext ): Future[Option[HalRepresentation]] = {
    resourceAs[HalRepresentation]
  }

  /**
    * Return the selected resource and return it in the specified type.
    * <p>
    *     If there are multiple matching representations, the first node is returned.
    * </p>
    * @param type the subtype of the HalRepresentation used to parse the resource.
    * @param <T> the subtype of HalRepresentation of the returned resource.
    * @return HalRepresentation
    * @throws IOException if a low-level I/O problem (unexpected end-of-input, network error) occurs.
    * @throws JsonParseException if the json document can not be parsed by Jackson's ObjectMapper
    * @throws JsonMappingException if the input JSON structure can not be mapped to the specified HalRepresentation type
    * @since 1.0.0
    */
  // no need? no
//  public <T extends HalRepresentation> Optional<T> getResourceAs(final Class<T> type) throws IOException {
//    return getResourceAs(type, null);
//  }

  /**
    * Return the selected resource and return it in the specified type.
    * <p>
    *     The EmbeddedTypeInfo is used to define the specific type of embedded items.
    * </p>
    * <p>
    *     If there are multiple matching representations, the first node is returned.
    * </p>
    * @param type the subtype of the HalRepresentation used to parse the resource.
    * @param embeddedTypeInfo specification of the type of embedded items
    * @param moreEmbeddedTypeInfos more type infors for embedded items
    * @param <T> the subtype of HalRepresentation of the returned resource.
    * @return HalRepresentation
    * @throws IOException if a low-level I/O problem (unexpected end-of-input, network error) occurs.
    * @throws JsonParseException if the json document can not be parsed by Jackson's ObjectMapper
    * @throws JsonMappingException if the input JSON structure can not be mapped to the specified HalRepresentation type
    * @since 1.0.0
    */
  // no need? no
//  public <T extends HalRepresentation> Optional<T> getResourceAs(final Class<T> type,
//  final EmbeddedTypeInfo embeddedTypeInfo,
//  final EmbeddedTypeInfo... moreEmbeddedTypeInfos) throws IOException {
//    if (moreEmbeddedTypeInfos == null || moreEmbeddedTypeInfos.length == 0) {
//      return getResourceAs(type, embeddedTypeInfo != null ? singletonList(embeddedTypeInfo) : emptyList());
//    } else {
//      final List<EmbeddedTypeInfo> typeInfos = new ArrayList<>();
//      typeInfos.add(requireNonNull(embeddedTypeInfo));
//      typeInfos.addAll(asList(moreEmbeddedTypeInfos));
//      return getResourceAs(type, typeInfos);
//    }
//  }

  /**
    * Retrieve the HAL resource identified by {@code uri} and return the representation as a HalRepresentation.
    *
    * @param link the Link of the resource to retrieve, or null, if the contextUrl should be resolved.
    * @throws IllegalArgumentException if resolving URLs is failing
    * @param resultType the Class of the returned HalRepresentation.
    * @param embeddedTypeInfo type information about embedded items
    * @param <T> type parameter of the returned HalRepresentation
    * @return list of zero or more HalRepresentations.
    */
  // no need? no
  //  private <T extends HalRepresentation> T getResource(final Link link,
  //  final Class<T> resultType,
  //  final EmbeddedTypeInfo embeddedTypeInfo) throws IOException {
  //    return getResource(link, resultType, singletonList(embeddedTypeInfo));
  //  }

  /**
    * Retrieve the HAL resource identified by {@code uri} and return the representation as a HalRepresentation.
    *
    * @param link the Link of the resource to retrieve, or null, if the contextUrl should be resolved.
    * @param type the expected type of the returned resource
    * @param <T> the type of the returned HalRepresentation
    * @return HalRepresentation
    * @throws IllegalArgumentException if resolving URLs is failing
    * @throws IOException if a low-level I/O problem (unexpected end-of-input, network error) occurs.
    */
  def resourceAs[T: EC: Decoder: HalDecoder: ClassTag]( link: Link ): Future[T] = {
    log.trace(
      s"Fetching resource href=${link.href} rel=${link.rel} as " +
      s"type=${classTag[T].runtimeClass.getSimpleName()}"
    )

    linkResolver( link )
      .flatMap { jsonDoc =>
        val result = for {
          json <- io.circe.parser.parse( jsonDoc )
          hal  <- json.as[HalRepresentation]
          r    <- hal.as[T]
        } yield r

        Future fromTry { result.toTry }
      }
  }
  //  private <T extends HalRepresentation> T getResource(final Link link,
  //  final Class<T> type,
  //  final List<EmbeddedTypeInfo> embeddedType) throws IOException {
  //    LOG.trace("Fetching resource href={} rel={} as type={} with embeddedType={}", link.getHref(), link.getRel(), type.getSimpleName(), embeddedType);
  //    final String json;
  //    try {
  //      json = linkResolver.apply(link);
  //    } catch (final IOException | RuntimeException e) {
  //      LOG.error("Failed to fetch resource href={}: {}", link.getHref(), e.getMessage());
  //      throw e;
  //    }
  //    try {
  //      return embeddedType != null && !embeddedType.isEmpty()
  //      ? parse(json, objectMapper).as(type, embeddedType)
  //      : parse(json, objectMapper).as(type);
  //    } catch (final RuntimeException e) {
  //      LOG.error("Failed to parse resource href={} rel={} as type={} with embeddedType={}", link.getHref(), link.getRel(), type.getSimpleName(), embeddedType);
  //      throw e;
  //    }
  //  }

  /**
    * Return the selected resource and return it in the specified type.
    * <p>
    *   The EmbeddedTypeInfo is used to define the specific type of embedded items.
    * </p>
    * <p>
    *   If there are multiple matching representations, the first node is returned.
    * </p>
    * @param type the subtype of the HalRepresentation used to parse the resource.
    * @param embeddedTypeInfos specification of the type of embedded items
    * @param <T> the subtype of HalRepresentation of the returned resource.
    * @return HalRepresentation
    * @throws IOException if a low-level I/O problem (unexpected end-of-input, network error) occurs.
    */
  def resourceAs[T: EC: HalDecoder]: Future[Option[T]] = {
    checkState()

    def traverseHops(): Future[Seq[HalRepresentation]] = {
      if (_startWith.isEmpty) traverseInitialResource( false )
      else traverseHop( this.accumulation.head, false )
    }

    def decodeHead( reps: Seq[HalRepresentation] ): Future[Option[T]] = {
      Future fromTry {
        reps.headOption
          .map { _.as[T] }
          .fold {
            Option.empty[T].asRight[Throwable]
          } { er =>
            er map { Option( _ ) }
          }
          .toTry
      }
    }

    for {
      reps   <- traverseHops()
      result <- decodeHead( reps )
    } yield result
  }

  /**
    *
    * @param resultType the Class of the returned HalRepresentation.
    * @param embeddedTypeInfo type information about embedded items
    * @param retrieveAll false if only a single resource is returned, true otherwise.
    * @param <T> type parameter of the returned HalRepresentation
    * @return list of zero or more HalRepresentations.
    * @throws IOException if a low-level I/O problem (unexpected end-of-input, network error) occurs.
    * @throws JsonParseException if the json document can not be parsed by Jackson's ObjectMapper
    * @throws JsonMappingException if the input JSON structure can not be mapped to the specified HalRepresentation type
    */
  private def traverseInitialResource(
    retrieveAll: Boolean
  )(
    implicit ec: ExecutionContext
  ): Future[Seq[HalRepresentation]] = {
    def liftToSeq[T]( resource: Future[T] ): Future[Seq[T]] = resource map { Seq( _ ) }

    checkState()

    /*
    #hops = N; N > 0
    max nesting-level in embeddedTypeInfo = M; M >= 0
     */
    val initial = Link self _startWith.get.toString
    log.trace( s"starting with ${_startWith}" )
    val newTraveson = this.copy( _startWith = None )

    if (newTraveson.hops.isEmpty) {
      /*
       * 0. N=0, M=0:
       * resource(startwith, pageType)
       */
      liftToSeq( resourceAs[HalRepresentation]( initial ) )
    } else {
      // Follow startWith URL, but have a look at the next hop, so we can parse the resource
      // with respect to pageType and embeddedTypeInfo:
      for {
        firstHop <- resourceAs[HalRepresentation]( initial )
        result   <- traverseHop( firstHop, retrieveAll )
      } yield result
    }
  }

  /**
    *
    * @param current HalRepresentation of the current hop
    * @param resultType the Class of the returned HalRepresentation.
    * @param embeddedTypeInfo type information about embedded items
    * @param retrieveAll false if only a single resource is returned, true otherwise.
    * @param <T> type parameter of the returned HalRepresentation
    * @return list of zero or more HalRepresentations.
    * @throws IOException if a low-level I/O problem (unexpected end-of-input, network error) occurs.
    */
  private def traverseHop(
    current: HalRepresentation,
    retrieveAll: Boolean
  )(
    implicit ec: ExecutionContext
  ): Future[Seq[HalRepresentation]] = {
    def shallow( h: Traverson.Hop, ts: Seq[Traverson.Hop] ): Future[Seq[HalRepresentation]] = {
      // the next hop could possibly be already available as an embedded object:
      val embeddedItems = current.embedded.itemsBy( h.rel )

      if (embeddedItems.isEmpty) Future.successful { Seq.empty[HalRepresentation] } else {
        log.trace( s"returning ${embeddedItems.size} embedded ${h.rel}" )
        if (ts.isEmpty) Future.successful { embeddedItems } else
          traverseHop( embeddedItems.head, retrieveAll )
      }
    }

    def deep(
      curHop: Hop,
      tailHops: Seq[Hop],
      ls: NonEmptyList[Link]
    ): Future[Seq[HalRepresentation]] = {
      this.contextUrl
        .map { ctxUrl =>
          val expandedLink = resolve( ctxUrl, expand( ls.head, curHop.templateParams ) )
          if (tailHops.isEmpty) {
            if (retrieveAll) {
              log.trace( s"following ${ls.size} ${curHop.rel} links" )
              Future sequence {
                ls.toList
                  .map { l =>
                    resourceAs[HalRepresentation](
                      resolve(
                        ctxUrl,
                        expand( l, curHop.templateParams )
                      )
                    )
                  }
              }
            } else {
              val newContextUrl = Traverson.linkToUrl( expandedLink ).toOption
              val newTraverson = this.copy( hops = tailHops, contextUrl = newContextUrl )
              newTraverson.resourceAs[HalRepresentation]( expandedLink ).map { rep =>
                Seq( rep )
              }
            }
          } else {
            // some more hops

            val newContextUrl = Traverson.linkToUrl( expandedLink ).toOption
            val newTraverse = this.copy( hops = tailHops, contextUrl = newContextUrl )
            for {
              resource <- newTraverse.resourceAs[HalRepresentation]( expandedLink )
              response <- newTraverse.traverseHop( resource, retrieveAll )
            } yield response
          }
        }
        .getOrElse { Future.successful { Seq.empty[HalRepresentation] } }
    }

    import cats.syntax.list._

    this.hops.headOption
      .map { currentHop =>
        val tailHops = this.hops.tail
        log.trace( s"following ${currentHop.rel}" )

        current.links
          .linksBy( currentHop.rel, currentHop.followLink )
          .toList
          .toNel
          .map { links =>
            deep( currentHop, tailHops, links )
          }
          .getOrElse {
            if (!currentHop.ignoreEmbedded) shallow( currentHop, tailHops )
            else {
              log.error(
                s"can not follow hop ${currentHop.rel}: no matching links found in " +
                s"resource ${current}"
              )
              Future.successful { Seq.empty[HalRepresentation] }
            }
          }
      }
      .getOrElse { Future.successful { Seq.empty[HalRepresentation] } }
  }

  /**
    * Resolved a link using the URL of the current resource and returns it as an absolute Link.
    *
    * @param contextUrl the URL of the current context
    * @param link optional link to follow
    * @return absolute Link
    * @throws IllegalArgumentException if resolving the link is failing
    */
  private def resolve( contextUrl: URL, link: Link ): Link = {
    require( Option( link ).isEmpty || link.templated, "Link must not be templated" )

    val resolved = {
      for {
        l    <- Option( link )
        href <- Traverson.resolveHref( contextUrl, l.href ).toOption
      } yield Link.fromPrototype( l ).withHref( href.toString ).build()
    }

    resolved getOrElse { Link.self( contextUrl.toString ) }
  }

  private def expand( link: Link, vars: Map[String, AnyRef] ): Link = {
    if (link.templated) {
      val href = UriTemplate.fromTemplate( link.href ).expand( vars.asJava )
      Link
        .fromPrototype( link )
        .withHref( href )
        .withRel( link.rel )
        .build()
    } else {
      link
    }
  }

  /**
    * Checks the current state of the Traverson.
    *
    * @throws IllegalStateException if some error occured during traversion
    */
  private def checkState(): Unit = {
    if (_startWith.isEmpty && accumulation.isEmpty) {
      throw new IllegalStateException( "please call startWith(uri) first." )
    }
  }
}

object Traverson {
  case class Hop(
    /** Link-relation type of the hop. */
    rel: String,
    followLink: Hop.FollowLink,
    /** URI-template variables used when following the hop. */
    templateParams: Map[String, AnyRef],
    /** Ignore possibly embedded items. */
    ignoreEmbedded: Boolean
  )

  object Hop {
    type FollowLink = Link => Boolean

    val always: FollowLink = (_) => true
  }

  val log = LoggerFactory.getLogger( classOf[Traverson] )

  /**
    * <p>
    *  Create a Traverson that is using the given {@link Function} to resolve a Link and return a
    *  HAL+JSON document.
    * </p>
    * <p>
    *  The function will be called by the Traverson, whenever a Link must be followed. The Traverson
    *  will take care of URI templates (templated links), so the implementation of the function can
    *  rely on the Link parameter to be not {@link Link#isTemplated() templated}.
    * </p>
    * <p>
    *  Typical implementations of the Function will rely on some HTTP client. Especially in this
    *  case, the function should take care of the link's {@link Link#getType() type} and
    *  {@link Link#getProfile()}, so the proper HTTP Accept header is used.
    * </p>
    * @param linkResolver A function that gets a Link of a resource and returns a HAL+JSON document.
    * @return Traverson
    */
  def makeWithResolver( linkResolver: LinkResolver ): Traverson = {
    Traverson( linkResolver = linkResolver )
  }

  /**
    * Resolved a link using the URL of the current resource and returns it as an absolute Link.
    *
    * @param contextUrl the URL of the current context
    * @param link optional link to follow
    * @return absolute Link
    * @throws IllegalArgumentException if resolving the link is failing
    */
  def resolve( contextUrl: URL, link: Link ): Link = {
    require( !link.templated, "link must not be templated." )
    Option( link )
      .map { l =>
        Link.fromPrototype( l ).withHref( resolveHref( contextUrl, l.href ).toString ).build()
      }
      .getOrElse { Link.self( contextUrl.toString ) }
  }

  def embeddedTypeInfoFor[T: ClassTag](
    hops: Seq[Hop],
    embeddedTypeInfo: Seq[EmbeddedTypeInfo]
  ): EmbeddedTypeInfo = {
    require( !hops.isEmpty, "Hopes must not be empty" )

    val last = EmbeddedTypeInfo.withEmbedded( hops.last.rel, embeddedTypeInfo: _* )

    hops.reverse.tail.foldLeft( last ) { ( acc, hop ) =>
      EmbeddedTypeInfo.withEmbedded[HalRepresentation]( hop.rel, acc )
    }
  }

  def linkToUrl( link: Link ): ErrorOr[URL] = {
    require(
      !link.templated,
      s"Unable to create URL from tenmplated link ${link}"
    )

    hrefToUrl( link.href )
  }

  def hrefToUrl( href: String ): ErrorOr[URL] = Either catchNonFatal { new URL( href ) }

  def resolveHref( contextUrl: URL, href: String ): ErrorOr[URL] = {
    Either catchNonFatal {
      Option( contextUrl )
        .map { new URL( _, href ) }
        .getOrElse { new URL( href ) }
    }
  }
}
