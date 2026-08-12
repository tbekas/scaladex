package scaladex.data

import java.nio.file.Files
import java.nio.file.Path

import scaladex.core.service.Storage

import com.typesafe.scalalogging.LazyLogging

/** Generates the Gatling feeder CSVs (consumed by the `loadtest` module) from the local index, so
  * every load-test request targets data that exists in a freshly `data/run init`-ed database.
  *
  * Runs as a `data` step, like `data/run init`:
  * {{{ sbt "data/run generateFeeders" }}}
  *
  * Reuses [[scaladex.infra.FilesystemStorage]] (via [[scaladex.core.service.Storage]]) to read the
  * index — no separate JSON parsing — and writes three files to the loadtest resources:
  *   - orgs_repos.csv    : organization,repository
  *   - artifacts.csv     : organization,repository,groupId,artifactId,version
  *   - search_terms.csv  : term
  */
object GenerateFeeders extends LazyLogging:

  private val maxArtifactsPerProject = 3

  private val curatedTerms = Seq(
    "akka", "cats", "http", "json", "spark", "test", "zio", "play", "circe", "doobie",
    "scala", "monix", "shapeless", "kafka", "slick", "sttp", "fs2", "tapir", "munit", "scalatest"
  )

  def run(storage: Storage, outputDir: Path): Unit =
    val orgsRepos = List.newBuilder[(String, String)]
    val artifacts = List.newBuilder[(String, String, String, String, String)]
    val repoNames = Set.newBuilder[String]

    // Only Ok projects: moved/renamed (or NotFound) projects redirect to references that are not
    // stored in the index, which would produce 404s and redirect loops in the load test.
    for (project, projectArtifacts, _) <- storage.loadAllProjects() if project.githubStatus.isOk do
      val org = project.reference.organization.value
      val repo = project.reference.repository.value
      if valid(org) && valid(repo) then
        orgsRepos += ((org, repo))
        repoNames += repo
        for artifact <- projectArtifacts.take(maxArtifactsPerProject) do
          val groupId = artifact.groupId.value
          val artifactId = artifact.artifactId.value
          val version = artifact.version.value
          if valid(groupId) && valid(artifactId) && valid(version) then
            artifacts += ((org, repo, groupId, artifactId, version))

    // sort for deterministic, diff-friendly output (loadAllProjects yields filesystem order); the
    // artifact sort is stable so artifacts keep their in-project order
    val orgsReposRows = orgsRepos.result().sorted
    val artifactsRows = artifacts.result().sortBy { case (org, repo, _, _, _) => (org, repo) }
    val terms = (curatedTerms ++ repoNames.result().toList.sorted).distinct

    Files.createDirectories(outputDir)
    writeCsv(
      outputDir.resolve("orgs_repos.csv"),
      "organization,repository",
      orgsReposRows.map((org, repo) => s"$org,$repo")
    )
    writeCsv(
      outputDir.resolve("artifacts.csv"),
      "organization,repository,groupId,artifactId,version",
      artifactsRows.map((org, repo, groupId, artifactId, version) => s"$org,$repo,$groupId,$artifactId,$version")
    )
    writeCsv(outputDir.resolve("search_terms.csv"), "term", terms)

    logger.info(s"Wrote feeders to $outputDir")
    logger.info(s"  orgs_repos.csv  : ${orgsReposRows.size} rows")
    logger.info(s"  artifacts.csv   : ${artifactsRows.size} rows")
    logger.info(s"  search_terms.csv: ${terms.size} rows")
  end run

  // The CSVs are unquoted, comma-separated; skip any value that would break that simple format.
  private def valid(s: String): Boolean = s.nonEmpty && !s.contains(",") && !s.contains("\n")

  private def writeCsv(path: Path, header: String, rows: Seq[String]): Unit =
    val content = (header +: rows).mkString("", "\n", "\n")
    Files.write(path, content.getBytes("UTF-8"))
end GenerateFeeders
