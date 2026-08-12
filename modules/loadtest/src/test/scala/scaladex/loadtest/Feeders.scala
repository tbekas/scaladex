package scaladex.loadtest

import io.gatling.core.Predef.*

/** CSV feeders generated from `small-index` by the `data` module's `GenerateFeeders`.
  *
  * Regenerate them with:
  * {{{ sbt "data/run generateFeeders" }}}
  *
  * Files live in `src/test/resources/data/` (Gatling resolves feeder paths from the classpath).
  */
object Feeders:
  // organization,repository
  val projects = csv("data/orgs_repos.csv").random
  // organization,repository,groupId,artifactId,version
  val artifacts = csv("data/artifacts.csv").random
  // term
  val searchTerms = csv("data/search_terms.csv").random
end Feeders
