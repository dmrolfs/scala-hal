package com.github.dmrolfs.scalahal.hal

/**
  * Helper class used to resolve CURIs in links and embedded items.
  */
case class Curies( curies: Seq[Link] ) {
  import journal._
  private val log = Logger[Curies]

  /**
    * Registers a CURI link in the Curies instance.
    *
    * @param curi the CURI
    * @throws IllegalArgumentException if the link-relation type of the link is not equal to 'curies'
    */
  def register( curi: Link ): Curies = {
    require( curi.rel == Curies.Rel, "Link must be a CURI" )

    log.debug( s"Curies: curies=[${curies.mkString( ", " )}]" )
    log.debug( s"curi = [${curi}]" )

    val alreadyRegistered = curies exists { _.href == curi.href }
    log.debug( s"alreadyRegistered = ${alreadyRegistered}" )

    val newCuries = if (!alreadyRegistered) {
      curies
    } else {
      val r1 = curies
        .filterNot { _.name == curi.name }

      log.debug( s"r1 = [${r1.mkString( ", " )}]" )

      val r2 = r1.collect {
        case c if c.name == curi.name => curi
        case c                        => c
      }

      log.debug( s"r2 = [${r2.mkString( ", " )}]" )
      r2
    }
    log.debug( s"newCuries = [${newCuries.mkString( ", " )}]" )

    this.copy( curies = newCuries :+ curi )
  }

  /**
    * Merges this Curies with another instance of Curies and returns the merged instance.
    *
    * @param other merged Curies
    * @return a merged copy of this and other
    */
  def mergeWith( that: Curies ): Curies = {
    log.debug( "MERGING..." )
    that.curies.foldLeft( this ) { _ register _ }
  }

  /**
    * Merges this Curies with another instance of Curies and returns the merged instance.
    *
    * @param other merged Curies
    * @return a merged copy of this and other
    */
  def ++( that: Curies ): Curies = this mergeWith that

  /**
    * Resolves a link-relation type (curied or full rel) and returns the curied form, or
    * the unchanged rel, if no matching CURI is registered.
    *
    * @param rel link-relation type
    * @return curied link-relation type
    */
  def resolve( rel: String ): String = {
    val template: Option[CuriTemplate] = CuriTemplate.matchingCuriTemplateFor( curies, rel )
    log.info( s"Curies: resolve template = ${template}" )
    template.map { _.curiedRelFrom( rel ) } getOrElse { rel }
  }

  def expand( rel: String ): String = {
    if (rel contains ":") {
      val name = rel.substring( 0, rel.indexOf( ':' ) )

      curies
        .filter { _.name == name }
        .headOption
        .map {
          _.hrefAsTemplate
            .set( "rel", rel.substring( rel.indexOf( ':' ) + 1 ) )
            .expand()
        }
        .getOrElse { rel }
    } else {
      rel
    }
  }
}

object Curies {
  val Rel = "curies"

  /**
    * an empty Curies without curi links.
    *
    * @return default Curies
    */
  val empty = Curies( curies = Seq.empty[Link] )

  /**
    * Creates Curies from some {@link Links}. CURIes contained in the Links are
    * {@link #register(Link) registered}.
    *
    * @param links Links possibly containing CURIes
    * @return Curies
    */
  def fromLinks( links: Links ): Curies = Curies( links.linksBy( Rel ) )

}
