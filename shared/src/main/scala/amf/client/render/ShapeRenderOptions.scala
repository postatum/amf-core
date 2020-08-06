package amf.client.render

import amf.client.resolve.ClientErrorHandler
import amf.client.resolve.ClientErrorHandlerConverter.ErrorHandlerConverter
import amf.core.errorhandling.UnhandledErrorHandler

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

/**
  * Json Schema options
  */
@JSExportAll
@JSExportTopLevel("render.ShapeRenderOptions")
class ShapeRenderOptions {

  private var documentation: Boolean     = true
  private var compactedEmission: Boolean = false
  private var schema: JSONSchemaVersion  = JSONSchemaVersions.UNSPECIFIED
  private var eh: ClientErrorHandler     = ErrorHandlerConverter.asClient(UnhandledErrorHandler)

  /** Remove documentation items as examples, descriptions, display names, etc. */
  def withoutDocumentation: ShapeRenderOptions = {
    documentation = false
    this
  }

  /** Render shape extracting common types to definitions. */
  def withCompactedEmission: ShapeRenderOptions = {
    compactedEmission = true
    this
  }

  def withErrorHandler(errorHandler: ClientErrorHandler): ShapeRenderOptions = {
    eh = errorHandler
    this
  }

  def withSchemaVersion(version: JSONSchemaVersion): ShapeRenderOptions = {
    schema = version
    this
  }

  def errorHandler: ClientErrorHandler = eh
  def isWithDocumentation: Boolean     = documentation
  def isWithCompactedEmission: Boolean = compactedEmission
  def schemaVersion: JSONSchemaVersion = schema
}

object ShapeRenderOptions {
  def apply(): ShapeRenderOptions = new ShapeRenderOptions()
}

@JSExportAll
@JSExportTopLevel("JSONSchemaVersions")
object JSONSchemaVersions {
  val UNSPECIFIED: JSONSchemaVersion = Unspecified
  val DRAFT_04: JSONSchemaVersion = JsonSchemaDraft4
  val DRAFT_07: JSONSchemaVersion = JsonSchemaDraft7
}

sealed trait JSONSchemaVersion
object Unspecified extends JSONSchemaVersion
object JsonSchemaDraft4 extends JSONSchemaVersion
object JsonSchemaDraft7 extends JSONSchemaVersion