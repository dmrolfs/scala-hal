package com.github.dmrolfs

import scala.concurrent.ExecutionContext
import cats.data.{ NonEmptyChain, ValidatedNec }

package object scalahal extends EncoderSyntax with DecoderSyntax {
  type EC[_] = ExecutionContext

  type AllIssuesOr[A] = ValidatedNec[Throwable, A]

  type AllErrorsOr[T] = Either[NonEmptyChain[Throwable], T]

  type ErrorOr[T] = Either[Throwable, T]
}
