package scaladex.core.util

import scala.concurrent.Future

import scaladex.core.util.ScalaExtensions.*

import org.scalatest.funspec.AsyncFunSpec
import org.scalatest.matchers.should.Matchers

class ScalaExtensionsTests extends AsyncFunSpec with Matchers:
  class AbortException extends Exception("abort")

  // Tolerate everything except the AbortException marker.
  given Tolerance with
    def decide(error: Throwable): Tolerance.Decision = error match
      case _: AbortException => Tolerance.Decision.Abort
      case _ => Tolerance.Decision.Tolerate

  describe("mapSyncTolerant") {
    it("keeps successes and recovers tolerated failures via the fallback, preserving order") {
      val tolerated = collection.mutable.ListBuffer.empty[Int]
      Seq(1, 2, 3, 4)
        .mapSyncTolerant {
          case 2 => Future.failed(new RuntimeException("boom"))
          case n => Future.successful(n * 10)
        } { (n, _) =>
          tolerated += n
          -1
        }
        .map { result =>
          result shouldBe Seq(10, -1, 30, 40)
          tolerated.toList shouldBe List(2)
        }
    }

    it("aborts the whole batch when Tolerance decides to abort") {
      val processed = collection.mutable.ListBuffer.empty[Int]
      recoverToExceptionIf[AbortException] {
        Seq(1, 2, 3)
          .mapSyncTolerant {
            case 2 => Future.failed(new AbortException)
            case n => Future.successful { processed += n; n }
          }((_, _) => 0)
      }.map { _ =>
        processed.toList shouldBe List(1)
      }
    }
  }
