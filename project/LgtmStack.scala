import org.testcontainers.dockerclient.DockerClientProviderStrategy
import org.testcontainers.grafana.LgtmStackContainer
import org.testcontainers.utility.MountableFile
import sbt._

import java.io.IOException
import java.net.{HttpURLConnection, URL}
import java.nio.file.Path
import java.util.concurrent.TimeoutException
import scala.collection.JavaConverters.*
import scala.collection.concurrent.TrieMap
import scala.collection.mutable

object LgtmStack extends AutoPlugin {

  private val GRAFANA_PORT = 3000
  private val OTLP_GRPC_PORT = 4317
  private val OTLP_HTTP_PORT = 4318
  private val LOKI_PORT = 3100
  private val TEMPO_PORT = 3200
  private val PROMETHEUS_PORT = 9090

  private val containers: mutable.Map[Path, LgtmStackContainer] = TrieMap.empty

  object autoImport {
    val startLgtm = taskKey[(Int, Int, Int)]("Connect to Lgtm stack or start an Lgtm stack container")
  }

  import autoImport._

  def settings(defaultGrafanaPort: Int = GRAFANA_PORT, defaultOltpGrpcPort: Int = OTLP_GRPC_PORT, defaultOltpHttpPort: Int = OTLP_HTTP_PORT): Seq[Setting[?]] = Seq(
    startLgtm := {
      import sbt.util.CacheImplicits._
      val baseDir = Keys.baseDirectory.value
      val rootDir = (LocalRootProject / Keys.baseDirectory).value
      val dashboardFile = rootDir / "observability" / "grafana" / "dashboards" / "scaladex-performance.json"
      val providerFile = rootDir / "observability" / "grafana" / "provisioning" / "dashboards" / "scaladex.yaml"
      val streams = Keys.streams.value
      val logger = streams.log

      if (canConnect(defaultGrafanaPort)) {
        logger.info(s"Lgtm stack available on port $defaultGrafanaPort")
        (defaultGrafanaPort, defaultOltpGrpcPort, defaultOltpHttpPort)
      } else {
        val store = streams.cacheStoreFactory.make("lgtm-container")
        val tracker = util.Tracked.lastOutput[Unit, (String, Int, Int, Int)](store) {
          case (_, None) =>
            startContainer(baseDir, dashboardFile, providerFile, logger)
          case (_, Some((containerId, grafanaPort, otlpGrpcPort, otlpHttpPort))) =>
            if (canConnect(grafanaPort)) {
              logger.info(s"Lgtm stack container already started on port $grafanaPort")
              (containerId, grafanaPort, otlpGrpcPort, otlpHttpPort)
            } else {
              Docker.kill(containerId)
              startContainer(baseDir, dashboardFile, providerFile, logger)
            }
        }
        val (_, grafanaPort, otlpGrpcPort, otlpHttpPort) = tracker(())
        (grafanaPort, otlpGrpcPort, otlpHttpPort)
      }
    },
    Keys.clean := {
      Keys.clean.value
      containers.get(Keys.baseDirectory.value.toPath).foreach(_.close())
      containers.remove(Keys.baseDirectory.value.toPath)
    }
  )

  private def startContainer(
      baseDir: File,
      dashboardFile: File,
      providerFile: File,
      logger: Logger
  ): (String, Int, Int, Int) = {
    CurrentThread.setContextClassLoader[DockerClientProviderStrategy]
    val container = new LgtmStackContainer("grafana/otel-lgtm")

    // provision the Scaladex performance dashboard so Grafana loads it on startup
    container.withCopyFileToContainer(
      MountableFile.forHostPath(providerFile.getAbsolutePath),
      "/otel-lgtm/grafana/conf/provisioning/dashboards/scaladex.yaml"
    )
    container.withCopyFileToContainer(
      MountableFile.forHostPath(dashboardFile.getAbsolutePath),
      "/otel-lgtm/dashboards/scaladex-performance.json"
    )

    val (grafanaPort, otlpGrpcPort, otlpHttpPort) =
      try {
        container.start()
        (
          container.getMappedPort(GRAFANA_PORT),
          container.getMappedPort(OTLP_GRPC_PORT),
          container.getMappedPort(OTLP_HTTP_PORT)
        )
      } catch {
        case e: Throwable =>
          logger.error(s"Error starting Lgtm stack container: {$e}")
          container.close()
          throw e
      }
    logger.info(
      s"Lgtm stack container started: Grafana on port $grafanaPort, OTLP gRPC on port $otlpGrpcPort, OTLP HTTP on port $otlpHttpPort"
    )
    containers(baseDir.toPath) = container
    (container.getContainerId, grafanaPort, otlpGrpcPort, otlpHttpPort)
  }

  private def canConnect(port: Int): Boolean = {
    val url = new URL(s"http://localhost:$port/api/health")
    try {
      val connection = url.openConnection().asInstanceOf[HttpURLConnection]
      connection.setConnectTimeout(200)
      connection.connect()
      val respCode = connection.getResponseCode
      connection.disconnect()
      respCode == HttpURLConnection.HTTP_OK
    } catch {
      case _: TimeoutException | _: IOException => false
    }
  }
}
