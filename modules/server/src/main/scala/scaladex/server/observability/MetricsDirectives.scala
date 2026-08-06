package scaladex.server.observability

import scaladex.infra.Telemetry

import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route

object MetricsDirectives:

  /** Wrap a route so every request records an `http.server.request.duration` histogram sample and a
    * span, labeled by method, normalized route, and response status.
    */
  def withHttpMetrics(inner: Route): Route =
    extractRequest { request =>
      val start = System.nanoTime()
      val method = request.method.value
      val route = normalizeRoute(request.uri.path.toString)
      val span = Telemetry.tracer.spanBuilder(s"HTTP $method $route").startSpan()
      mapResponse { response =>
        Telemetry.recordHttp(method, route, response.status.intValue, (System.nanoTime() - start) / 1e9)
        span.setAttribute("http.request.method", method)
        span.setAttribute("http.route", route)
        span.setAttribute("http.response.status_code", response.status.intValue.toLong)
        span.end()
        response
      }(inner)
    }

  /** Best-effort mapping of a concrete request path to a low-cardinality route template.
    *
    * endpoints4s does not expose the matched template at runtime, so this collapses the known
    * Scaladex path families (org/repo/artifact segments) to placeholders to keep metric label
    * cardinality bounded.
    */
  private[observability] def normalizeRoute(rawPath: String): String =
    val path = if rawPath.isEmpty then "/" else rawPath
    path.split("/").iterator.filter(_.nonEmpty).toList match
      case Nil               => "/"
      case "assets" :: _     => "/assets/**"
      case "badges" :: _     => "/badges/**"
      case "callback" :: _   => "/callback/**"
      case "api" :: rest     => "/api/" + normalizeApi(rest)
      case single :: Nil     => s"/$single"
      case _ :: _ :: Nil     => "/{org}/{repo}"
      case _ :: _ :: _       => "/{org}/{repo}/**"

  private def normalizeApi(segments: List[String]): String =
    segments match
      case version :: "projects" :: _ :: _ :: rest =>
        val base = s"${ver(version)}/projects/{org}/{repo}"
        if rest.isEmpty then base else s"$base/**"
      case version :: "projects" :: Nil =>
        s"${ver(version)}/projects"
      case version :: tail =>
        (ver(version) :: tail.take(1)).mkString("/") + (if tail.sizeIs > 1 then "/**" else "")
      case Nil => "**"

  private def ver(v: String): String = if v.matches("v\\d+") then "v{n}" else v
end MetricsDirectives
