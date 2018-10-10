package com.github.dmrolfs.scalahal

import scala.language.implicitConversions
import com.github.dmrolfs.scalahal.hal.{ HalRepresentation, Link, Links }
import io.circe.Json

trait HalEncoder[T] extends (T => HalRepresentation )

object HalEncoder {
  def apply[T]( implicit e: HalEncoder[T] ): HalEncoder[T] = e

  def pure[T]( encoderFn: T => HalRepresentation ): HalEncoder[T] = {
    SimpleHalEncoder( fn = encoderFn )
  }

  private case class SimpleHalEncoder[T]( fn: T => HalRepresentation ) extends HalEncoder[T] {
    override def apply( o: T ): HalRepresentation = fn( o )
  }
}

trait EncoderSyntax {
  implicit final def syntaxEncodeHal[T: HalEncoder]( o: T ): HalEncodeOps[T] = new HalEncodeOps( o )
}

final class HalEncodeOps[T: HalEncoder]( val o: T ) {
  def asHal: HalRepresentation = HalEncoder[T].apply( o )
}
