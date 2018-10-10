package com.github.dmrolfs.scalahal.paging

import cats.syntax.apply._
import cats.syntax.validated._
import com.damnhandy.uri.template.UriTemplate
import com.github.dmrolfs.scalahal.hal.Links
import com.github.dmrolfs.scalahal.{ paging, AllIssuesOr }

/**
  * A helper class used to create paging links for paged resources that are using page URIs with
  * skip and limit paramters.
  * <p>
  *   By default, SkipLimitPaging is expecting an UriTemplate having the template variables 'skip'
  *   and 'limit' to create links to 'self', 'first', 'next', 'prev' and 'last' pages. If you want
  *   to use different var names, you should derive from this class and override {@link #skipVar()}
  *   and/or {@link #limitVar()}.
  * </p>
  * <p>
  *   As specified in <a href="https://tools.ietf.org/html/draft-kelly-json-hal-06#section-4.1.1">Section 4.1.1</a>
  *   of the HAL specification, the {@code _links} object <em>"is an object whose property names are
  *   link relation types (as defined by [RFC5988]) and values are either a Link Object or an array
  *   of Link Objects"</em>.
  * </p>
  * <p>
  *   Paging links like 'first', 'next', and so on should generally be rendered as single Link
  *   Objects, so adding these links to an resource should be done using {@link Links.Builder#single(List)}.
  * </p>
  * <p>
  *   Usage:
  * </p>
  * <pre><code>
  * public class MyHalRepresentation extends HalRepresentation {
  *   public MyHalRepresentation(final SkipLimitPaging page, final List&lt;Stuff&gt; pagedStuff) {
  *     super(linkingTo()
  *       .single(page.links(
  *         fromTemplate("http://example.com/api/stuff{?skip,limit}"),
  *         EnumSet.allOf(PagingRel.class)))
  *       .array(pagedStuff
  *         .stream()
  *         .limit(page.limit)
  *         .map(stuff -&gt; linkBuilder("item", stuff.href).withTitle(stuff.title).build())
  *         .collect(toList()))
  *       .build()
  *     )
  *   }
  * }
  * </code></pre>
  */
case class SkipLimitPaging private (
  /**
    * The number of items to skip.
    */
  skip: Int,
  /**
    * The number of items per page.
    */
  limit: Int,
  /**
    * There are more items beyond the current page - or not.
    */
  hasMore: Boolean,
  /**
    * Optional total number of available items. Used to calculate the number of items to skip for the
    * last page.
    */
  total: Option[Int]
) {

  /**
    * Return the requested links for a paged resource were the link's hrefs are created using the
    * given {@link UriTemplate}.
    * <p>
    *   The variables used to identify the number of skipped items and page size must match the
    *   values returned by {@link #skipVar()} ()} and {@link #limitVar()}. Derive from this class,
    *   if other values than {@link #SKIP_VAR} or {@link #LIMIT_VAR} are required.
    * </p>
    * <p>
    *   If the provided template does not contain the required variable names. links can not be
    *   expanded.
    * </p>
    * @param template the URI template used to create paging links.
    * @param rels the links expected to be created.
    * @return paging links
    */
  def links( template: UriTemplate, rels: Set[PagingRel] ): Links = {
    import PagingRel.{ FIRST, LAST, NEXT, PREV, SELF }
    import com.github.dmrolfs.scalahal.hal.Link.link

    val builder = rels.foldLeft( Links.linkingTo ) { ( b, rel ) =>
      rel match {
        case SELF => b.self( pageUri( template, skip, limit ) )

        case FIRST => b.single( link( rel.toString, pageUri( template, 0, limit ) ) )

        case PREV if 0 < skip => {
          b.single(
            link( rel.toString, pageUri( template, math.max( 0, skip - limit ), limit ) )
          )
        }

        case NEXT if hasMore => {
          b.single( link( rel.toString, pageUri( template, skip + limit, limit ) ) )
        }

        case LAST if total.isDefined => {
          val s = calcLastPageSkip( total.get, skip, limit )
          b.single( link( rel.toString, pageUri( template, s, limit ) ) )
        }

        case _ => b
      }
    }

    builder.build()
  }

  /**
    * Return the name of the template variable used to specify the number of skipped items.
    * <p>
    *     Override this method if you want to use links with a different name than 'skip'
    *     for the number of skipped items.
    * </p>
    *
    * @return template variable for skipped items.
    */
  protected def skipVar: String = SkipLimitPaging.SKIP_VAR

  /**
    * Return the name of the template variable used to specify the size of the page.
    * <p>
    *     Override this methode if you want to use links with a different name than 'limit'
    *     for the number of items per page.
    * </p>
    *
    * @return template variable for the page size.
    */
  protected def limitVar: String = paging.SkipLimitPaging.LIMIT_VAR

  /**
    * Calculate the number of items to skip for the last page.
    *
    * @param total total number of items.
    * @param skip number of items to skip for the current page.
    * @param limit page size
    * @return skipped items
    */
  def calcLastPageSkip( total: Int, skip: Int, limit: Int ): Int = {
    if (total - limit < skip) skip
    else if (0 < total % limit) total - total % limit
    else total - limit
  }

  /**
    * Return the URI of the page with N skipped items and a page limitted to pages of size M.
    * @param uriTemplate the template used to create the link
    * @param skip the number of skipped items
    * @param limit the page size
    * @return href
    */
  def pageUri( template: UriTemplate, skip: Int, limit: Int ): String = {
    if (limit == Int.MaxValue) template.expand()
    else template.set( skipVar, skip ).set( limitVar, limit ).expand()
  }

}

object SkipLimitPaging {

  /**
    * The default template-variable name used to identify the number of items to skip
    */
  val SKIP_VAR: String = "skip"

  /**
    * The default template-variable name used to identify the number of items per page
    */
  val LIMIT_VAR: String = "limit"

  /**
    * Create a SkipLimitPaging instances for pages where it is known whether or not there are more
    * items beyond the current page.
    *
    * @param skip the number of items to skip before the current page
    * @param limit the number of items per page.
    * @param hasMore true if there are more items beyond the current page, false otherwise.
    * @return created SkipLimitPaging instance
    */
  def makeWithoutTotal( skip: Int, limit: Int, hasMore: Boolean ): AllIssuesOr[SkipLimitPaging] = {
    (
      checkSkip( skip ),
      checkLimit( limit ),
      checkUnbounded( hasMore, limit )
    ) mapN {
      case ( s, l, ( hm, _ ) ) =>
        SkipLimitPaging(
          skip = s,
          limit = l,
          hasMore = hm,
          total = None
        )
    }
  }

  /**
    * Create a SkipLimitPaging instances for pages where it is known how many items are matching the
    * initial query.
    *
    * @param skip the number of items to skip to come to this page.
    * @param limit the number of items per page.
    * @param totalCount the total number of items matching the initial query.
    * @return created SkipLimitPaging instance
    */
  def makeWithTotal( skip: Int, limit: Int, totalCount: Int ): AllIssuesOr[SkipLimitPaging] = {
    (
      checkSkip( skip ),
      checkLimit( limit ),
      checkTotalGreaterThanZero( totalCount ),
      checkTotalGreaterThanSkip( totalCount, skip )
    ) mapN {
      case ( s, l, t, _ ) =>
        SkipLimitPaging(
          skip = s,
          limit = l,
          hasMore = s * l < t,
          total = Some( t )
        )
    }
  }

  private def checkSkip( skip: Int ): AllIssuesOr[Int] = {
    if (0 <= skip) skip.validNec
    else {
      new IllegalArgumentException(
        s"paramtere 'skip' [${skip}] must not be less than zero"
      ).invalidNec
    }
  }

  private def checkLimit( limit: Int ): AllIssuesOr[Int] = {
    if (0 < limit) limit.validNec
    else {
      new IllegalArgumentException(
        s"parameter 'limit' [${limit}] must be greater than zero"
      ).invalidNec
    }
  }

  private def checkUnbounded( hasMore: Boolean, limit: Int ): AllIssuesOr[( Boolean, Int )] = {
    if (!hasMore || limit != Int.MaxValue) ( hasMore, limit ).validNec
    else {
      new IllegalArgumentException(
        "unable to calculate next page for unbounded page sizes"
      ).invalidNec
    }
  }

  private def checkTotalGreaterThanZero( totalCount: Int ): AllIssuesOr[Int] = {
    if (0 <= totalCount) totalCount.validNec
    else {
      new IllegalArgumentException(
        s"parameter 'totalCount' [${totalCount}] must be greater than or equal to zero"
      ).invalidNec
    }
  }

  private def checkTotalGreaterThanSkip( totalCount: Int, skip: Int ): AllIssuesOr[( Int, Int )] = {
    if (skip <= totalCount) ( totalCount, skip ).validNec
    else {
      new IllegalArgumentException(
        s"parameter 'totalCount' [${totalCount}] must be greater than 'skip' [${skip}]"
      ).invalidNec
    }
  }
}
