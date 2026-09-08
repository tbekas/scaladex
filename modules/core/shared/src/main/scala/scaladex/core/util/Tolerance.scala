package scaladex.core.util

/** Decides, for a failed item in a tolerant batch, whether to skip it and continue or abort the whole batch. */
trait Tolerance:
  def decide(error: Throwable): Tolerance.Decision

object Tolerance:
  enum Decision:
    case Tolerate, Abort
