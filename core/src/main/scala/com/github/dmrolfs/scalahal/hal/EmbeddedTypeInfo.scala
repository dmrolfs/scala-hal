package com.github.dmrolfs.scalahal.hal

import scala.reflect.{ classTag, ClassTag }

case class EmbeddedTypeInfo private (
  rel: String,
  embeddedType: Class[_],
  nestedTypeInfo: Seq[EmbeddedTypeInfo]
) {
  override def toString: String = {
    s"EmbeddedTypeInfo{rel='${rel}', type=${embeddedType.getSimpleName()}, " +
    s"nestedTypeInfo=${nestedTypeInfo}}"
  }
}

object EmbeddedTypeInfo {

  def withEmbedded[T: ClassTag](
    rel: String,
    nestedTypeInfo: EmbeddedTypeInfo*
  ): EmbeddedTypeInfo = {
    val embeddedType = classTag[T].runtimeClass
    if (nestedTypeInfo == null || nestedTypeInfo.isEmpty) {
      EmbeddedTypeInfo( rel, embeddedType, Seq.empty[EmbeddedTypeInfo] )
    } else {
      EmbeddedTypeInfo( rel, embeddedType, nestedTypeInfo )
    }
  }
//
//  def withEmbedded(
//    rel: String,
//    embeddedType: Class[_],
//    nestedTypeInfo: Seq[EmbeddedTypeInfo]
//  ): EmbeddedTypeInfo = {
//    EmbeddedTypeInfo( rel, embeddedType, nestedTypeInfo )
//  }
}
