package com.github.dmrolfs.scalahal.traverson

/**
  * Typically used together with `Future` to signal completion
  * but there is no actual value completed. More clearly signals intent
  * than `Unit` and is available both from Scala and Java (which `Unit` is not).
  * forked from Akka.Done
  */
sealed abstract class Done

case object Done extends Done {

  /** Java API - singleton instance
    */
  def getInstance(): Done = this
}
