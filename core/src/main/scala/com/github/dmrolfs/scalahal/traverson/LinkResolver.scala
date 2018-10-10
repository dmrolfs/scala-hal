package com.github.dmrolfs.scalahal.traverson

import scala.concurrent.Future
import com.github.dmrolfs.scalahal.hal.Link

/**
  * Functional interface used to resolve a {@link de.otto.edison.hal.Link} and return the
  * {@code application/hal+json} resource as a String.
  * <p>
  *   The function will be called by the Traverson, whenever a Link must be followed. The Traverson
  *   will take care of URI templates (templated links), so the implementation of the function can
  *   rely on the Link parameter to be not {@link Link#isTemplated() templated}.
  * </p>
  * <p>
  *   Typical implementations of the Function will rely on some HTTP client. Especially in this
  *   case, the function should take care of the link's {@link Link#getType() type} and
  *   {@link Link#getProfile()}, so the proper HTTP Accept header is used.
  * </p>
  */
trait LinkResolver extends (Link => Future[String] )
