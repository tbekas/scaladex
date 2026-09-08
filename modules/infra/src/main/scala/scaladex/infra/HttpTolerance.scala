package scaladex.infra

import scala.util.control.NonFatal

import scaladex.core.util.Tolerance

import org.apache.pekko.pattern.CircuitBreakerOpenException

/** Tolerance policy for HTTP-backed batch jobs: an open circuit breaker aborts the job, any other non-fatal error
  * skips the offending item, and fatal errors abort.
  */
object HttpTolerance:
  given Tolerance with
    def decide(error: Throwable): Tolerance.Decision = error match
      case _: CircuitBreakerOpenException => Tolerance.Decision.Abort
      case NonFatal(_) => Tolerance.Decision.Tolerate
      case _ => Tolerance.Decision.Abort
