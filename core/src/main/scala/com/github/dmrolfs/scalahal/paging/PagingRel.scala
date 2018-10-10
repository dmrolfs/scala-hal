package com.github.dmrolfs.scalahal.paging

import scala.collection.immutable
import enumeratum._

/**
  * Link-relation types used in paged resources.
  *
  * @see <a href="http://www.iana.org/assignments/link-relations/link-relations.xhtml">IANA link-relations</a>
  */
sealed trait PagingRel extends EnumEntry {
  override def toString: String = entryName.toLowerCase
}

object PagingRel extends Enum[PagingRel] {
  override val values: immutable.IndexedSeq[PagingRel] = findValues

  /** Conveys an identifier for the link's context. */
  case object SELF extends PagingRel

  /** An IRI that refers to the furthest preceding resource in a series of resources. */
  case object FIRST extends PagingRel

  /** Indicates that the link's context is a part of a series, and that the previous in the series
    * is the link target. */
  case object PREV extends PagingRel

  /** Indicates that the link's context is a part of a series, and that the next in the series is
    * the link target. */
  case object NEXT extends PagingRel

  /** An IRI that refers to the furthest following resource in a series of resources. */
  case object LAST extends PagingRel
}
