package com.github.dmrolfs.scalahal.paging

import cats.syntax.apply._
import cats.syntax.validated._
import com.damnhandy.uri.template.UriTemplate
import com.github.dmrolfs.scalahal.AllIssuesOr
import com.github.dmrolfs.scalahal.hal.{ Link, Links }

/**
  * A helper class used to create paging links for paged resources that are using numbered page
  * URIs.
  * <p>
  *   By default, NumberedPaging is expecting an UriTemplate having the template variables 'page'
  *   and 'pageSize' to create links to 'self', 'first', 'next', 'prev' and 'last' pages. If you
  *   want to use different var names, you should derive from this class and override
  *   {@link #pageNumberVar()} and/or {@link #pageSizeVar()}.
  * </p>
  * <p>
  *   Both zero- and one-based paging is supported.
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
  *   public MyHalRepresentation(final NumberedPaging page, final List&lt;Stuff&gt; pagedStuff) {
  *     super(linkingTo()
  *       .single(page.links(
  *         fromTemplate("http://example.com/api/stuff{?pageNumber,pageSize}"),
  *         EnumSet.allOf(PagingRel.class)))
  *       .array(pagedStuff
  *         .stream()
  *         .limit(page.pageNumber*pageSize)
  *         .map(stuff -&gt; linkBuilder("item", stuff.href).withTitle(stuff.title).build())
  *         .collect(toList()))
  *       .build()
  *     );
  *   }
  * }
  * </code></pre>
  */
case class NumberedPaging private (
  /**
    * The number of the first page. Must be 0 or 1.
    */
  firstPage: Int,
  /**
    * The page number of the current page.
    */
  pageNumber: Int,
  /**
    * The size of the pages.
    */
  pageSize: Int,
  /**
    * More items beyond the current page - or not.
    */
  hasMore: Boolean,
  /**
    * The optional total number of available items in the current selection.
    * <p>
    *   Used to calculate the 'last' page. If this value is empty(), the link to the last page is
    *   not returned by {@link #links(UriTemplate, EnumSet)}.
    * </p>
    */
  total: Option[Int]
) {

  /**
    * Return the requested links for a paged resource were the link's hrefs are created using the
    * given {@link UriTemplate}.
    * <p>
    *   The variables used to identify the current page number and page size must match the values
    *   returned by {@link #pageNumberVar()} and {@link #pageSizeVar()}. Derive from this class, if
    *   other values than {@link #PAGE_NUMBER_VAR} or {@link #PAGE_SIZE_VAR} are required.
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

    val builder = rels.foldLeft( Links.linkingTo ) { ( b, rel ) =>
      rel match {
        case SELF => b.self( pageUri( template, pageNumber, pageSize ) )

        case FIRST => {
          b.single( Link.link( rel.toString, pageUri( template, firstPage, pageSize ) ) )
        }

        case PREV if firstPage < pageNumber => {
          b.single(
            Link.link( rel.toString, pageUri( template, pageNumber - 1, pageSize ) )
          )
        }

        case NEXT if hasMore => {
          b.single(
            Link.link( rel.toString, pageUri( template, pageNumber + 1, pageSize ) )
          )
        }

        case LAST if total.isDefined => {
          b.single(
            Link.link(
              rel.toString,
              pageUri(
                template,
                calcLastPage( total.get, pageSize ),
                pageSize
              )
            )
          )
        }

        case _ => b
      }
    }

    builder.build()
  }

  /**
    * Returns number of the last page, or {@code None} if {@link #total} is unknown / empty.
    *
    * @return the number of the last page.
    */
  def lastPage: Option[Int] = total map { t =>
    calcLastPage( t, pageSize )
  }

  /**
    * The name of the uri-template variable used to identify the current page number. By default,
    * 'page' is used.
    * @return uri-template variable for the current page-number.
    */
  protected def pageNumberVar: String = NumberedPaging.PAGE_NUMBER_VAR

  /**
    * The name of the uri-template variable used to specify the current page size. By default,
    * 'pageSize' is used.
    * @return uri-template variable for the current page-size.
    */
  protected def pageSizeVar: String = NumberedPaging.PAGE_SIZE_VAR

  /**
    * Returns the number of the last page, if the total number of items is known.
    *
    * @param total total number of items
    * @param pageSize the current page size
    * @return page number of the last page
    */
  private def calcLastPage( total: Int, pageSize: Int ): Int = {
    if (total == 0) firstPage
    else {
      val zeroBasedPageNo = {
        if (total % pageSize > 0) total / pageSize
        else total / pageSize - 1
      }

      firstPage + zeroBasedPageNo
    }
  }

  /**
    * Return the HREF of the page specified by UriTemplate, pageNumber and pageSize.
    *
    * @param uriTemplate the template used to build hrefs.
    * @param pageNumber the number of the linked page.
    * @param pageSize the size of the pages.
    * @return href of the linked page.
    */
  private def pageUri( uriTemplate: UriTemplate, pageNumber: Int, pageSize: Int ): String = {
    if (pageSize == Int.MaxValue) uriTemplate.expand()
    else uriTemplate.set( pageNumberVar, pageNumber ).set( pageSizeVar, pageSize ).expand()
  }
}

object NumberedPaging {

  /**
    * The default template-variable name used to identify the number of the page.
    */
  val PAGE_NUMBER_VAR = "page"

  /**
    * The default template-variable name used to identify the size of the page.
    */
  val PAGE_SIZE_VAR = "pageSize"

  /**
    * Create a NumberedPaging instances for pages where paging starts with zero and it is known whether or
    * not there are more items beyond the current page.
    *
    * @param pageNumber the page number of the current page.
    * @param pageSize the number of items per page.
    * @param hasMore true if there are more items beyond the current page, false otherwise.
    * @return created NumberedPaging instance
    */
  def zeroBased( pageNumber: Int, pageSize: Int, hasMore: Boolean ): AllIssuesOr[NumberedPaging] = {
    makeWithoutTotal(
      firstPage = 0,
      pageNumber = pageNumber,
      pageSize = pageSize,
      hasMore = hasMore
    )
  }

  /**
    * Create a NumberedPaging instances for pages where paging starts with zero and it is known
    * how many items are matching the initial query.
    *
    * @param pageNumber the page number of the current page.
    * @param pageSize the number of items per page.
    * @param totalCount the total number of items matching the initial query.
    * @return created NumberedPaging instance
    */
  def zeroBasedWithTotal(
    pageNumber: Int,
    pageSize: Int,
    totalCount: Int
  ): AllIssuesOr[NumberedPaging] = {
    makeWithTotal(
      firstPage = 0,
      pageNumber = pageNumber,
      pageSize = pageSize,
      total = totalCount
    )
  }

  /**
    * Create a NumberedPaging instances for pages where paging starts with one and it is known whether
    * or not there are more items beyond the current page.
    *
    * @param pageNumber the page number of the current page.
    * @param pageSize the number of items per page.
    * @param hasMore true if there are more items beyond the current page, false otherwise.
    * @return created NumberedPaging instance
    */
  def oneBased( pageNumber: Int, pageSize: Int, hasMore: Boolean ): AllIssuesOr[NumberedPaging] = {
    makeWithoutTotal(
      firstPage = 1,
      pageNumber = pageNumber,
      pageSize = pageSize,
      hasMore = hasMore
    )
  }

  /**
    * Create a NumberedPaging instances for pages where paging starts with one and it is known how
    * many items are matching the initial query.
    *
    * @param pageNumber the page number of the current page.
    * @param pageSize the number of items per page.
    * @param totalCount the total number of items matching the initial query.
    * @return created NumberedPaging instance
    */
  def oneBasedWithTotal(
    pageNumber: Int,
    pageSize: Int,
    totalCount: Int
  ): AllIssuesOr[NumberedPaging] = {
    makeWithTotal(
      firstPage = 1,
      pageNumber = pageNumber,
      pageSize = pageSize,
      total = totalCount
    )
  }

  private def makeWithoutTotal(
    firstPage: Int,
    pageNumber: Int,
    pageSize: Int,
    hasMore: Boolean
  ): AllIssuesOr[NumberedPaging] = {
    (
      checkPageCommon( firstPage, pageNumber, pageSize ),
      checkUnbounded( hasMore, pageSize )
    ) mapN {
      case ( ( fp, pn, ps ), ( hm, _ ) ) =>
        NumberedPaging(
          firstPage = fp,
          pageNumber = pn,
          pageSize = ps,
          hasMore = hm,
          total = None
        )
    }
  }

  private def makeWithTotal(
    firstPage: Int,
    pageNumber: Int,
    pageSize: Int,
    total: Int
  ): AllIssuesOr[NumberedPaging] = {
    (
      checkPageCommon( firstPage, pageNumber, pageSize ),
      checkTotal( total )
    ) mapN {
      case ( ( fp, pn, ps ), t ) =>
        val offset = if (fp == 0) 1 else 0

        NumberedPaging(
          firstPage = fp,
          pageNumber = pn,
          pageSize = ps,
          hasMore = (pn + offset) * ps < t,
          total = Some( t )
        )
    }
  }

  private def checkPageCommon(
    firstPage: Int,
    pageNumber: Int,
    pageSize: Int
  ): AllIssuesOr[( Int, Int, Int )] = {
    (
      checkFirstPage( firstPage ),
      checkPageNumber( pageNumber, firstPage ),
      checkPageSize( pageSize )
    ) mapN { ( f, n, s ) =>
      ( f, n, s )
    }
  }

  private def checkFirstPage( firstPage: Int ): AllIssuesOr[Int] = {
    if (firstPage == 0 || firstPage == 1) firstPage.validNec
    else {
      new IllegalArgumentException(
        s"parameter 'firstPage' [${firstPage}] must be 0 or 1"
      ).invalidNec
    }
  }

  private def checkPageNumber( pageNumber: Int, firstPage: Int ): AllIssuesOr[Int] = {
    if (firstPage <= pageNumber) pageNumber.validNec
    else {
      new IllegalArgumentException(
        s"parameter 'pageNumber' [${pageNumber}] must not be less than ${firstPage}"
      ).invalidNec
    }
  }

  private def checkPageSize( pageSize: Int ): AllIssuesOr[Int] = {
    if (0 < pageSize) pageSize.validNec
    else {
      new IllegalArgumentException(
        s"parameter 'pageSize' [${pageSize}] must be greater than 0"
      ).invalidNec
    }
  }

  private def checkTotal( total: Int ): AllIssuesOr[Int] = {
    if (0 <= total) total.validNec
    else {
      new IllegalArgumentException(
        s"parameter 'total' [${total}] must be greater than or equal to 0"
      ).invalidNec
    }
  }

  private def checkUnbounded( hasMore: Boolean, pageSize: Int ): AllIssuesOr[( Boolean, Int )] = {
    if (!hasMore || pageSize != Int.MaxValue) ( hasMore, pageSize ).validNec
    else {
      new IllegalArgumentException(
        "unable to calculate next page for unbounded page sizes"
      ).invalidNec
    }
  }
}
