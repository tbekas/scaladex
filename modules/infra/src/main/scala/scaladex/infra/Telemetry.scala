package scaladex.infra

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import com.typesafe.scalalogging.LazyLogging
import io.opentelemetry.api.GlobalOpenTelemetry
import io.opentelemetry.api.common.Attributes
import io.opentelemetry.api.metrics.DoubleHistogram
import io.opentelemetry.api.trace.StatusCode
import io.opentelemetry.api.trace.Tracer
import io.opentelemetry.sdk.autoconfigure.AutoConfiguredOpenTelemetrySdk

/** Central OpenTelemetry holder.
  *
  * The SDK is initialized only when an OTLP endpoint is configured (see [[init]]). When it is not,
  * `GlobalOpenTelemetry` stays the built-in no-op and every instrument below resolves to a cheap
  * no-op, so non-local environments incur ~zero overhead and produce no export errors.
  *
  * Instruments are `lazy val`s that read from `GlobalOpenTelemetry`, so as long as [[init]] runs
  * before the first request is served (it is called at the top of `Server.main`), load order versus
  * class-loaded objects such as `DoobieUtils` is irrelevant.
  */
object Telemetry extends LazyLogging:

  def init(): Unit =
    val endpointConfigured =
      sys.props.get("otel.exporter.otlp.endpoint").exists(_.nonEmpty) ||
        sys.env.get("OTEL_EXPORTER_OTLP_ENDPOINT").exists(_.nonEmpty)
    val disabled = sys.props.get("otel.sdk.disabled").contains("true")
    if endpointConfigured && !disabled then
      Try {
        val autoConfigured = AutoConfiguredOpenTelemetrySdk.builder().setResultAsGlobal().build()
        val sdk = autoConfigured.getOpenTelemetrySdk
        sys.addShutdownHook(sdk.close())
        logger.info("OpenTelemetry SDK initialized")
      }.recover { case e => logger.error("Failed to initialize OpenTelemetry SDK", e) }
    else logger.info("OpenTelemetry endpoint not configured; telemetry disabled (no-op)")
  end init

  private lazy val meter = GlobalOpenTelemetry.getMeter("scaladex")
  lazy val tracer: Tracer = GlobalOpenTelemetry.getTracer("scaladex")

  private lazy val httpServerDuration: DoubleHistogram =
    meter
      .histogramBuilder("http.server.request.duration")
      .setUnit("s")
      .setDescription("Duration of HTTP server requests")
      .build()

  private lazy val dbClientDuration: DoubleHistogram =
    meter
      .histogramBuilder("db.client.operation.duration")
      .setUnit("s")
      .setDescription("Duration of Postgres statement execution")
      .build()

  private lazy val searchDuration: DoubleHistogram =
    meter
      .histogramBuilder("search.request.duration")
      .setUnit("s")
      .setDescription("Duration of Elasticsearch requests")
      .build()

  def recordHttp(method: String, route: String, statusCode: Int, seconds: Double): Unit =
    val attrs = Attributes
      .builder()
      .put("http.request.method", method)
      .put("http.route", route)
      .put("http.response.status_code", statusCode.toLong)
      .build()
    httpServerDuration.record(seconds, attrs)

  def recordDb(operation: String, table: String, outcome: String, seconds: Double): Unit =
    val attrs = Attributes
      .builder()
      .put("db.operation", operation)
      .put("db.sql.table", table)
      .put("outcome", outcome)
      .build()
    dbClientDuration.record(seconds, attrs)

  def recordSearch(operation: String, outcome: String, seconds: Double): Unit =
    val attrs = Attributes
      .builder()
      .put("operation", operation)
      .put("outcome", outcome)
      .build()
    searchDuration.record(seconds, attrs)

  /** Wrap a `Future` in a span that starts now and ends when the future completes. Sets the span
    * status to ERROR and records the exception on failure.
    */
  def span[A](name: String)(f: => Future[A])(using ExecutionContext): Future[A] =
    val span = tracer.spanBuilder(name).startSpan()
    val scope = span.makeCurrent()
    val result =
      try f
      finally scope.close()
    result.onComplete {
      case Success(_) => span.end()
      case Failure(e) =>
        span.setStatus(StatusCode.ERROR, e.getMessage)
        span.recordException(e)
        span.end()
    }
    result
  end span
end Telemetry
