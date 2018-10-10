package com.github.dmrolfs.scalahal.hal

/**
  * Predicates used to select links matching some criteria.
  *
  * @since 0.1.0
  */
trait LinkPredicate extends (Link => Boolean )

object LinkPredicate {

  /**
    * Returns a Predicate that is matching every link.
    *
    * @return Predicate used to select links
    */
  val always: LinkPredicate = new LinkPredicate {
    override def apply( link: Link ): Boolean = true
  }

  /**
    * Returns a Predicate that is matching links having the specified type
    * {@link Link#getType() type}
    *
    * @param type the expected media type of the link
    * @return Predicate used to select links
    */
  def havingType( `type`: String ): LinkPredicate = new LinkPredicate {
    override def apply( link: Link ): Boolean = Some( `type` ) contains link.`type`
  }

  /**
    * Returns a Predicate that is matching links having the specified type
    * {@link Link#getType() type}, or no type at all.
    *
    * @param type the expected media type of the link
    * @return Predicate used to select links
    */
  def optionallyHavingType( `type`: String ): LinkPredicate = new LinkPredicate {
    override def apply( link: Link ): Boolean = {
      link.`type`.isEmpty || havingType( `type` )( link )
    }
  }

  /**
    * Returns a Predicate that is matching links having the specified profile
    * {@link Link#getProfile() profile}
    *
    * @param profile the expected profile of the link
    * @return Predicate used to select links
    */
  def havingProfile( profile: String ): LinkPredicate = new LinkPredicate {
    override def apply( link: Link ): Boolean = optionally( profile ) == link.profile
  }

  /**
    * Returns a Predicate that is matching links having the specified profile
    * {@link Link#getProfile() profile}, or no profile at all.
    *
    * @param profile the expected profile of the link
    * @return Predicate used to select links
    */
  def optionallyHavingProfile( profile: String ): LinkPredicate = new LinkPredicate {
    override def apply( link: Link ): Boolean = {
      link.profile.isEmpty || havingProfile( profile )( link )
    }
  }

  /**
    * Returns a Predicate that is matching links having the specified name
    * {@link Link#getName() name}
    *
    * @param name the expected name of the link
    * @return Predicate used to select links
    */
  def havingName( name: String ): LinkPredicate = new LinkPredicate {
    override def apply( link: Link ): Boolean = {
      optionally( name ) == link.name
    }
  }

  /**
    * Returns a Predicate that is matching links having the specified name
    * {@link Link#getName() name}, or no name at all.
    *
    * @param name the expected name of the link
    * @return Predicate used to select links
    */
  def optionallyHavingName( name: String ): LinkPredicate = new LinkPredicate {
    override def apply( link: Link ): Boolean = {
      link.name.isEmpty || havingName( name )( link )
    }
  }

  private def optionally( str: String ): Option[String] = Option( str ).filter { !_.isEmpty }
}
