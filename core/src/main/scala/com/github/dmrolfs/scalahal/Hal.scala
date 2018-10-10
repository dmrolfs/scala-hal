package com.github.dmrolfs.scalahal

import com.github.dmrolfs.scalahal.hal.HalRepresentation

case class Hal[T: HalDecoder]( underlying: HalRepresentation ) {}
