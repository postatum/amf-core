package amf.core.emitter

import amf.client.render.{JSONSchemaVersion, JSONSchemaVersions, ShapeRenderOptions => ClientShapeRenderOptions}
import amf.client.resolve.ClientErrorHandlerConverter._
import amf.core.errorhandling.{ErrorHandler, UnhandledErrorHandler}

/**
  * JSON Schema options
  */
class ShapeRenderOptions {

  private var documentation: Boolean = true
  private var compactedEmission: Boolean = false
  private var schema: JSONSchemaVersion = JSONSchemaVersions.UNSPECIFIED

  private var eh: ErrorHandler       = UnhandledErrorHandler

  /** Remove documentation info as examples, descriptions, display names, etc. */
  def withoutDocumentation: ShapeRenderOptions = {
    documentation = false
    this
  }

  /** Render shape extracting common types to definitions */
  def withCompactedEmission: ShapeRenderOptions = {
    compactedEmission = true
    this
  }

  def withErrorHandler(errorHandler: ErrorHandler): ShapeRenderOptions = {
    eh = errorHandler
    this
  }

  def withSchemaVersion(version: JSONSchemaVersion): ShapeRenderOptions = {
    schema = version
    this
  }

  def isWithDocumentation: Boolean = documentation
  def isWithCompactedEmission: Boolean = compactedEmission
  def errorHandler: ErrorHandler   = eh
  def schemaVersion: JSONSchemaVersion = schema
}

object ShapeRenderOptions {
  def apply(): ShapeRenderOptions = new ShapeRenderOptions()

  def apply(client: ClientShapeRenderOptions): ShapeRenderOptions = {
    val opts = new ShapeRenderOptions()
    opts.documentation = client.isWithDocumentation
    opts.compactedEmission = client.isWithCompactedEmission
    opts.eh = ErrorHandlerConverter.asInternal(client.errorHandler)
    opts.withSchemaVersion(client.schemaVersion)
    opts
  }
}
