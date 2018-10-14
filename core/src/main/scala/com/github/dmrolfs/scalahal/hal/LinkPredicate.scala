package com.github.dmrolfs.scalahal.hal

import journal._

/**
  * Predicates used to select links matching some criteria.
  *
  * @since 0.1.0
  */
sealed abstract class LinkPredicate extends (Link => Boolean ) { self =>

  /**
    * Returns a composed predicate that represents a short-circuiting logical
    * AND of this predicate and another.  When evaluating the composed
    * predicate, if this predicate is {@code false}, then the {@code other}
    * predicate is not evaluated.
    *
    * <p>Any exceptions thrown during evaluation of either predicate are relayed
    * to the caller; if evaluation of this predicate throws an exception, the
    * {@code other} predicate will not be evaluated.
    *
    * @param other a predicate that will be logically-ANDed with this
    *              predicate
    * @return a composed predicate that represents the short-circuiting logical
    * AND of this predicate and the {@code other} predicate
    */
  def and( other: LinkPredicate ): LinkPredicate = LinkPredicate.pure { link =>
    self( link ) && other( link )
  }

  /**
    * Returns a predicate that represents the logical negation of this
    * predicate.
    *
    * @return a predicate that represents the logical negation of this
    * predicate
    */
  def negate: LinkPredicate = LinkPredicate.pure { link =>
    !self( link )
  }

  /**
    * Returns a composed predicate that represents a short-circuiting logical
    * OR of this predicate and another.  When evaluating the composed
    * predicate, if this predicate is {@code true}, then the {@code other}
    * predicate is not evaluated.
    *
    * <p>Any exceptions thrown during evaluation of either predicate are relayed
    * to the caller; if evaluation of this predicate throws an exception, the
    * {@code other} predicate will not be evaluated.
    *
    * @param other a predicate that will be logically-ORed with this
    *              predicate
    * @return a composed predicate that represents the short-circuiting logical
    * OR of this predicate and the {@code other} predicate
    * @throws NullPointerException if other is null
    */
  def or( other: LinkPredicate ): LinkPredicate = LinkPredicate.pure { link =>
    self( link ) || other( link )
  }
}

object LinkPredicate {
  val log = Logger[LinkPredicate]

  def pure( predicate: Link => Boolean ): LinkPredicate = Simple( predicate )

  /**
    * Returns a Predicate that is matching every link.
    *
    * @return Predicate used to select links
    */
  val always: LinkPredicate = pure { _ =>
    true
  }

  /**
    * Returns a Predicate that is matching links having the specified type
    * {@link Link#getType() type}
    *
    * @param type the expected media type of the link
    * @return Predicate used to select links
    */
  def havingType( `type`: String ): LinkPredicate = pure { _.`type` contains `type` }

  /**
    * Returns a Predicate that is matching links having the specified type
    * {@link Link#getType() type}, or no type at all.
    *
    * @param type the expected media type of the link
    * @return Predicate used to select links
    */
  def optionallyHavingType( `type`: String ): LinkPredicate = pure { link =>
    link.`type`.isEmpty || havingType( `type` )( link )
  }

  /**
    * Returns a Predicate that is matching links having the specified profile
    * {@link Link#getProfile() profile}
    *
    * @param profile the expected profile of the link
    * @return Predicate used to select links
    */
  def havingProfile( profile: String ): LinkPredicate = pure { _.profile == optionally( profile ) }

  /**
    * Returns a Predicate that is matching links having the specified profile
    * {@link Link#getProfile() profile}, or no profile at all.
    *
    * @param profile the expected profile of the link
    * @return Predicate used to select links
    */
  def optionallyHavingProfile( profile: String ): LinkPredicate = pure { link =>
    link.profile.isEmpty || havingProfile( profile )( link )
  }

  /**
    * Returns a Predicate that is matching links having the specified name
    * {@link Link#getName() name}
    *
    * @param name the expected name of the link
    * @return Predicate used to select links
    */
  def havingName( name: String ): LinkPredicate = pure { _.name == optionally( name ) }

  /**
    * Returns a Predicate that is matching links having the specified name
    * {@link Link#getName() name}, or no name at all.
    *
    * @param name the expected name of the link
    * @return Predicate used to select links
    */
  def optionallyHavingName( name: String ): LinkPredicate = pure { link =>
    link.name.isEmpty || havingName( name )( link )
  }

  private def optionally( str: String ): Option[String] = Option( str ) filter { !_.isEmpty }

  private case class Simple( predicate: Link => Boolean ) extends LinkPredicate {
    override def apply( l: Link ): Boolean = predicate( l )
  }

}
