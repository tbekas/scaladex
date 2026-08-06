package scaladex.infra.sql

import scala.concurrent.ExecutionContext
import scaladex.infra.config.PostgreSQLConfig
import cats.effect.*
import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import doobie.*
import doobie.hikari.HikariTransactor
import doobie.util.log.{ExecFailure, LogHandler, ProcessingFailure, Success}
import org.flywaydb.core.Flyway

import com.typesafe.scalalogging.LazyLogging
import scaladex.infra.Telemetry

object DoobieUtils extends LazyLogging:

  private given ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  enum SqlOperation(val label: String):
    case Select extends SqlOperation("SELECT")
    case Update extends SqlOperation("UPDATE")
    case Insert extends SqlOperation("INSERT")
    case Delete extends SqlOperation("DELETE")

  /** Some builders receive a FROM-clause fragment (aliases, JOINs, possibly paren-wrapped) as their
    * `table` argument rather than a bare table name. Reduce it to the primary table name so it is a
    * usable, low-cardinality metric label.
    */
  private def primaryTable(table: String): String =
    table.dropWhile(c => c == '(' || c.isWhitespace).takeWhile(c => c.isLetterOrDigit || c == '_')

  def telemetryLogHandler(table: String, op: SqlOperation): LogHandler =
    val tableLabel = primaryTable(table)
    LogHandler {

    case Success(s, a, e1, e2) =>
      Telemetry.recordDb(op.label, tableLabel, "success", (e1.toNanos + e2.toNanos) / 1e9)
      logger.debug(s"""Successful Statement Execution:
                     |
                     |  ${s.linesIterator.dropWhile(_.trim.isEmpty).mkString("\n  ")}
                     |
                     | arguments = [${a.mkString(", ").take(1000) + (if a.size > 1000 then "..." else "")}]
                     |   elapsed = ${e1.toMillis.toString} ms exec + ${e2.toMillis.toString} ms processing (${(e1 + e2).toMillis.toString} ms total)
        """.stripMargin)

    case ProcessingFailure(s, a, e1, e2, t) =>
      Telemetry.recordDb(op.label, tableLabel, "processing_failure", (e1.toNanos + e2.toNanos) / 1e9)
      logger.error(
        s"""Failed Resultset Processing:
           |
           |  ${s.linesIterator.dropWhile(_.trim.isEmpty).mkString("\n  ")}
           |
           | arguments = [${a.mkString(", ").take(1000) + (if a.size > 1000 then "..." else "")}]
           |   elapsed = ${e1.toMillis.toString} ms exec + ${e2.toMillis.toString} ms processing (failed) (${(e1 + e2).toMillis.toString} ms total)
          """.stripMargin,
        t
      )

    case ExecFailure(s, a, e1, t) =>
      Telemetry.recordDb(op.label, tableLabel, "exec_failure", e1.toNanos / 1e9)
      logger.error(
        s"""Failed Statement Execution:
           |
           |  ${s.linesIterator.dropWhile(_.trim.isEmpty).mkString("\n  ")}
           |
           | arguments = [${a.mkString(", ").take(1000) + (if a.size > 1000 then "..." else "")}]
           |   elapsed = ${e1.toMillis.toString} ms exec (failed)
          """.stripMargin,
        t
      )
  }
  end telemetryLogHandler

  def flyway(conf: PostgreSQLConfig): Flyway =
    val datasource = getHikariDataSource(conf)
    flyway(datasource)

  def flyway(datasource: HikariDataSource): Flyway =
    Flyway
      .configure()
      .dataSource(datasource)
      .locations("migrations", "scaladex/infra/migrations")
      .load()

  def getHikariDataSource(conf: PostgreSQLConfig): HikariDataSource =
    val config: HikariConfig = new HikariConfig()
    config.setDriverClassName(conf.driver)
    config.setJdbcUrl(conf.url)
    config.setUsername(conf.user)
    config.setPassword(conf.pass.decode)
    new HikariDataSource(config)
  end getHikariDataSource

  def transactor(datasource: HikariDataSource): Resource[IO, HikariTransactor[IO]] =
    for
      ce <- ExecutionContexts.fixedThreadPool[IO](32) // our connect EC
      be <- Blocker[IO] // our blocking EC
    yield Transactor.fromDataSource[IO](datasource, ce, be)

  def insertOrUpdateRequest[T: Write](
      table: String,
      insertFields: Seq[String],
      onConflictFields: Seq[String],
      updateFields: Seq[String] = Seq.empty
  ): Update[T] =
    val insert = insertRequest(table, insertFields).sql
    val onConflictFieldsStr = onConflictFields.mkString(",")
    val action = if updateFields.nonEmpty then updateFields.mkString(" UPDATE SET ", " = ?, ", " = ?") else "NOTHING"
    given LogHandler = telemetryLogHandler(table, SqlOperation.Insert)
    Update(s"$insert ON CONFLICT ($onConflictFieldsStr) DO $action")
  end insertOrUpdateRequest

  def insertRequest[T: Write](table: String, fields: Seq[String]): Update[T] =
    val fieldsStr = fields.mkString(", ")
    val valuesStr = fields.map(_ => "?").mkString(", ")
    given LogHandler = telemetryLogHandler(table, SqlOperation.Insert)
    Update(s"INSERT INTO $table ($fieldsStr) VALUES ($valuesStr)")

  def updateRequest[T: Write](table: String, fields: Seq[String], keys: Seq[String]): Update[T] =
    val fieldsStr = fields.map(f => s"$f=?").mkString(", ")
    val keysStr = keys.map(k => s"$k=?").mkString(" AND ")
    given LogHandler = telemetryLogHandler(table, SqlOperation.Update)
    Update(s"UPDATE $table SET $fieldsStr WHERE $keysStr")

  def updateRequest0[T: Write](table: String, set: Seq[String], where: Seq[String]): Update[T] =
    val setStr = set.mkString(", ")
    val whereStr = where.mkString(" AND ")
    given LogHandler = telemetryLogHandler(table, SqlOperation.Update)
    Update(s"UPDATE $table SET $setStr WHERE $whereStr")

  def selectRequest[A: Read](table: String, fields: Seq[String]): Query0[A] =
    val fieldsStr = fields.mkString(", ")
    Query0(s"SELECT $fieldsStr FROM $table", logHandler = telemetryLogHandler(table, SqlOperation.Select))

  def selectRequest[A: Write, B: Read](table: String, fields: Seq[String], keys: Seq[String]): Query[A, B] =
    val fieldsStr = fields.mkString(", ")
    val keysStr = keys.map(k => s"$k=?").mkString(" AND ")
    Query(
      s"SELECT $fieldsStr FROM $table WHERE $keysStr",
      logHandler0 = telemetryLogHandler(table, SqlOperation.Select)
    )

  def selectRequest[A: Read](
      table: String,
      fields: Seq[String],
      where: Seq[String] = Seq.empty,
      groupBy: Seq[String] = Seq.empty,
      orderBy: Option[String] = None,
      limit: Option[Long] = None
  ): Query0[A] =
    val fieldsStr = fields.mkString(", ")
    val whereStr = if where.nonEmpty then where.mkString(" WHERE ", " AND ", "") else ""
    val groupByStr = if groupBy.nonEmpty then groupBy.mkString(" GROUP BY ", ", ", "") else ""
    val orderByStr = orderBy.map(o => s" ORDER BY $o").getOrElse("")
    val limitStr = limit.map(l => s" LIMIT $l").getOrElse("")
    Query0(
      s"SELECT $fieldsStr FROM $table" + whereStr + groupByStr + orderByStr + limitStr,
      logHandler = telemetryLogHandler(table, SqlOperation.Select)
    )
  end selectRequest

  def selectRequest1[A: Write, B: Read](
      table: String,
      fields: Seq[String],
      keys: Seq[String] = Seq.empty,
      where: Seq[String] = Seq.empty,
      groupBy: Seq[String] = Seq.empty,
      orderBy: Option[String] = None,
      limit: Option[Long] = None
  ): Query[A, B] =
    val fieldsStr = fields.mkString(", ")
    val allWhere = keys.map(k => s"$k=?") ++ where
    val whereStr = if allWhere.nonEmpty then allWhere.mkString(" WHERE ", " AND ", "") else ""
    val groupByStr = if groupBy.nonEmpty then groupBy.mkString(" GROUP BY ", ", ", "") else ""
    val orderByStr = orderBy.map(o => s" ORDER BY $o").getOrElse("")
    val limitStr = limit.map(l => s" LIMIT $l").getOrElse("")
    Query(
      s"SELECT $fieldsStr FROM $table" + whereStr + groupByStr + orderByStr + limitStr,
      logHandler0 = telemetryLogHandler(table, SqlOperation.Select)
    )
  end selectRequest1

  def deleteRequest[T: Write](table: String, where: Seq[String]): Update[T] =
    val whereK = where.map(k => s"$k=?").mkString(" AND ")
    given LogHandler = telemetryLogHandler(table, SqlOperation.Delete)
    Update(s"DELETE FROM $table WHERE $whereK")
end DoobieUtils
