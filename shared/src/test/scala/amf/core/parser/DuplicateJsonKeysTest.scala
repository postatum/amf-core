package amf.core.parser

import amf.Core
import amf.client.convert.NativeOps
import amf.client.parse.DefaultParserErrorHandler
import amf.core.remote.{Cache, Context}
import amf.core.services.RuntimeCompiler
import amf.core.unsafe.PlatformSecrets
import org.scalatest.{AsyncFunSuite, Matchers}

import scala.concurrent.ExecutionContext

trait DuplicateJsonKeysTest extends AsyncFunSuite with PlatformSecrets with NativeOps with Matchers{

  override implicit def executionContext: ExecutionContext = ExecutionContext.Implicits.global

  test("Parsed JSON with duplicate keys has several warnings") {
    Core.init().asFuture.flatMap { _ =>
      val errorHandler = DefaultParserErrorHandler.withRun()
      val url = "file://shared/src/test/resources/parser/duplicate-key.json"
      RuntimeCompiler(url, None, None, base = Context(platform), cache = Cache(), errorHandler = errorHandler).map {
        _ =>
          val errors = errorHandler.getErrors
          errors.size should be(4)
          val allAreDuplicateKeyWarnings = errors.forall(r => r.completeMessage.contains("Duplicate key") && r.level.contains("Warning"))
          allAreDuplicateKeyWarnings shouldBe true
      }
    }
  }
}
