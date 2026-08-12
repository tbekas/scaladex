package scaladex.loadtest

import scala.concurrent.duration.*

import io.gatling.core.Predef.*

/** Injection profiles and their tunables (system properties), shared by all simulations.
  *
  * Browsing profile (sustained realistic load):
  *   -Dloadtest.rate=5            new sessions per second at steady state
  *   -Dloadtest.rampUp=30         seconds to ramp up to `rate`
  *   -Dloadtest.duration=300      seconds to hold `rate`
  *
  * Stress profiles (ramp-to-failure / constant):
  *   -Dloadtest.maxRate=100       requests/sec at the top of the ramp (or the constant rate)
  *   -Dloadtest.rampDuration=200  seconds of the ramp (or the constant phase)
  */
object Stress:
  val maxRate: Double = sys.props.getOrElse("loadtest.maxRate", "100").toDouble
  val rampDuration: Int = sys.props.getOrElse("loadtest.rampDuration", "100").toInt
  val constantDuration: Int = sys.props.getOrElse("loadtest.constantDuration", "100").toInt

  /** Ramp-to-failure: linearly increase from 1 to `maxRate` over `rampDuration`. */
  def ramp = rampUsersPerSec(1).to(maxRate).during(rampDuration.seconds)

  /** Constant load at `maxRate` for `rampDuration`. */
  def constant = constantUsersPerSec(maxRate).during(constantDuration.seconds)

  def rampThenConstant = Seq(ramp, constant)
end Stress
