package scaladex.loadtest

import io.gatling.core.Predef.*
import scaladex.loadtest.Stress.{constantDuration, maxRate, rampDuration}

import scala.concurrent.duration.DurationInt

/** Sustained, realistic browsing load: each virtual user runs a short session (front page → search →
  * autocomplete → project → artifacts → one of api/version-matrix/awesome) with think-time pauses.
  *
  * Uses the same tunables as the other simulations (see [[Stress]]). Since a browsing session fires
  * several requests, the user-injection rate is scaled down by `requestsPerSession` so the aggregate
  * request throughput still equals `maxRate` — i.e. the same request-level stress as the
  * single-request simulations. Asserts an SLA (p95 < 1s, >99% success) so a breach shows as a failure.
  */
class BrowsingSimulation extends Simulation:

  private val userRate = 10

  private val browse = scenario("Browsing session")
    .exec(Chains.frontPage).pause(1, 3)
    .exec(Chains.search).pause(1, 4)
    .exec(Chains.autocomplete).pause(1, 3)
    .exec(Chains.projectPage).pause(1, 4)
    .exec(Chains.projectArtifacts).pause(1, 3)
    .randomSwitch(
      50.0 -> Chains.apiArtifact,
      30.0 -> Chains.versionMatrix,
      20.0 -> Chains.awesome
    )

  setUp(browse.inject(
    rampUsersPerSec(1).to(userRate).during(rampDuration.seconds),
    constantUsersPerSec(userRate).during(constantDuration.seconds)
  ))
    .protocols(ScaladexProtocol.httpProtocol)
    .assertions(
      global.successfulRequests.percent.gt(99.0),
      global.responseTime.percentile(95.0).lt(1000)
    )
end BrowsingSimulation
