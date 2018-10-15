package com.github.dmrolfs.scalahal

package object hal {
  type SingleOrArray[T] = Either[T, Seq[T]]

  object SingleOrArray {
    def isArray( item: SingleOrArray[_] ): Boolean = item.isRight
  }

  type RelPair[T] = ( String, SingleOrArray[T] )
}
