package com.github.dmrolfs.scalahal

import scala.language.implicitConversions
import com.github.dmrolfs.scalahal.hal.HalRepresentation

trait HalDecoder[T] extends (HalRepresentation => HalDecoder.Result[T] )

object HalDecoder {
  type Result[T] = ErrorOr[T]

  def apply[T]( implicit d: HalDecoder[T] ): HalDecoder[T] = d

  def pure[T]( decoderFn: HalRepresentation => Result[T] ): HalDecoder[T] = {
    SimpleHalDecoder( fn = decoderFn )
  }

  private case class SimpleHalDecoder[T](
    fn: HalRepresentation => Result[T]
  ) extends HalDecoder[T] {
    override def apply( hal: HalRepresentation ): Result[T] = fn( hal )
  }
}

trait DecoderSyntax {
  implicit final def syntaxDecodeHal( rep: HalRepresentation ): HalDecodeOps = {
    new HalDecodeOps( rep )
  }
}

final class HalDecodeOps( val rep: HalRepresentation ) extends AnyVal {
  def as[T: HalDecoder]: HalDecoder.Result[T] = HalDecoder[T].apply( rep )
}
