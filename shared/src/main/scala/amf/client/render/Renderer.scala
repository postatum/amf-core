package amf.client.render

import java.io.{File, Writer}

import amf.client.convert.CoreClientConverters._
import amf.client.environment.Environment
import amf.client.model.document.BaseUnit
import amf.core.AMFSerializer
import amf.core.emitter.{
  RenderOptions => InternalRenderOptions,
  ShapeRenderOptions => InternalShapeRenderOptions
}
import amf.core.model.document.{BaseUnit => InternalBaseUnit}
import amf.core.unsafe.PlatformSecrets
import org.mulesoft.common.io.Output._
import org.mulesoft.common.io.{LimitedStringBuffer, Output}
import org.yaml.builder.DocBuilder

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js.annotation.JSExport

/**
  * Base class for a renderer.
  */
class Renderer(val vendor: String, val mediaType: String, private val env: Option[Environment] = None)
    extends PlatformSecrets {

  private implicit val executionContext: ExecutionContext = env match {
    case Some(environment) => environment.executionEnvironment.executionContext
    case None              => platform.defaultExecutionEnvironment.executionContext
  }

  /**
    * Asynchronously renders the syntax text and stores it in the file pointed by the provided URL.
    * It must throw an UnsupportedOperation exception in platforms without support to write to the file system
    * (like the browser) or if a remote URL is provided.
    */
  def generateFile(unit: BaseUnit, output: File): ClientFuture[Unit] = {
    generateFile(unit, output, RenderOptions(), ShapeRenderOptions())
  }

  /**
    * Asynchronously renders the syntax text and stores it in the file pointed by the provided URL.
    * It must throw an UnsupportedOperation exception in platforms without support to write to the file system
    * (like the browser) or if a remote URL is provided.
    */
  def generateFile(unit: BaseUnit, output: File, options: RenderOptions): ClientFuture[Unit] = {
    generateFile(unit, "file://" + output.getAbsolutePath, options, ShapeRenderOptions())
  }

  /**
    * Asynchronously renders the syntax text and stores it in the file pointed by the provided URL.
    * It must throw an UnsupportedOperation exception in platforms without support to write to the file system
    * (like the browser) or if a remote URL is provided.
    */
  def generateFile(unit: BaseUnit, output: File, shapeOptions: ShapeRenderOptions): ClientFuture[Unit] = {
    generateFile(unit, "file://" + output.getAbsolutePath, RenderOptions(), shapeOptions)
  }

  /**
    * Asynchronously renders the syntax text and stores it in the file pointed by the provided URL.
    * It must throw an UnsupportedOperation exception in platforms without support to write to the file system
    * (like the browser) or if a remote URL is provided.
    */
  def generateFile(unit: BaseUnit, output: File, options: RenderOptions, shapeOptions: ShapeRenderOptions): ClientFuture[Unit] = {
    generateFile(unit, "file://" + output.getAbsolutePath, options, shapeOptions)
  }

  /**
    * Asynchronously renders the syntax text and stores it in the file pointed by the provided URL.
    * It must throw an UnsupportedOperation exception in platforms without support to write to the file system
    * (like the browser) or if a remote URL is provided.
    */
  @JSExport
  def generateFile(unit: BaseUnit, url: String): ClientFuture[Unit] = {
    generateFile(unit, url, RenderOptions(), ShapeRenderOptions())
  }

  /**
    * Asynchronously renders the syntax text and stores it in the file pointed by the provided URL.
    * It must throw an UnsupportedOperation exception in platforms without support to write to the file system
    * (like the browser) or if a remote URL is provided.
    */
  @JSExport
  def generateFile(unit: BaseUnit, url: String, options: RenderOptions, shapeOptions: ShapeRenderOptions): ClientFuture[Unit] =
    generate(unit._internal, url, InternalRenderOptions(options), InternalShapeRenderOptions(shapeOptions)).asClient

  /** Asynchronously renders the syntax text and returns it. */
  @JSExport
  def generateString(unit: BaseUnit): ClientFuture[String] = {
    generateString(unit, RenderOptions(), ShapeRenderOptions())
  }

  /** Asynchronously renders the syntax text and returns it. */
  @JSExport
  def generateString(unit: BaseUnit, options: RenderOptions): ClientFuture[String] = {
    generateString(unit, options, ShapeRenderOptions())
  }

  /** Asynchronously renders the syntax text and returns it. */
  @JSExport
  def generateString(unit: BaseUnit, shapeOptions: ShapeRenderOptions): ClientFuture[String] = {
    generateString(unit, RenderOptions(), shapeOptions)
  }

  /** Asynchronously renders the syntax text and returns it. */
  @JSExport
  def generateString(unit: BaseUnit, options: RenderOptions, shapeOptions: ShapeRenderOptions): ClientFuture[String] =
    generate(unit._internal, InternalRenderOptions(options), InternalShapeRenderOptions(shapeOptions)).asClient

  /** Asynchronously renders the syntax to a provided writer and returns it. */
  @JSExport
  def generateToWriter(unit: BaseUnit, options: RenderOptions, writer: Writer): ClientFuture[Unit] =
    generateToWriter(unit, options, ShapeRenderOptions(), writer)

  /** Asynchronously renders the syntax to a provided writer and returns it. */
  @JSExport
  def generateToWriter(unit: BaseUnit, shapeOptions: ShapeRenderOptions, writer: Writer): ClientFuture[Unit] =
    generateToWriter(unit, RenderOptions(), shapeOptions, writer)

  /** Asynchronously renders the syntax to a provided writer and returns it. */
  @JSExport
  def generateToWriter(unit: BaseUnit, options: RenderOptions, shapeOptions: ShapeRenderOptions, writer: Writer): ClientFuture[Unit] =
    generate(unit._internal, InternalRenderOptions(options), InternalShapeRenderOptions(shapeOptions), writer).asClient

  /** Asynchronously renders the syntax to a provided writer and returns it. */
  @JSExport
  def generateToWriter(unit: BaseUnit, writer: Writer): ClientFuture[Unit] =
    generateToWriter(unit, RenderOptions(), ShapeRenderOptions(), writer)

  /** Asynchronously renders the syntax to a provided string buffer with limited capacity and returns it. */
  @JSExport
  def generateToWriter(unit: BaseUnit, options: RenderOptions, writer: LimitedStringBuffer): ClientFuture[Unit] =
    generateToWriter(unit, options, ShapeRenderOptions(), writer)

  /** Asynchronously renders the syntax to a provided string buffer with limited capacity and returns it. */
  @JSExport
  def generateToWriter(unit: BaseUnit, shapeOptions: ShapeRenderOptions, writer: LimitedStringBuffer): ClientFuture[Unit] =
    generateToWriter(unit, RenderOptions(), shapeOptions, writer)

  /** Asynchronously renders the syntax to a provided string buffer with limited capacity and returns it. */
  @JSExport
  def generateToWriter(unit: BaseUnit, options: RenderOptions, shapeOptions: ShapeRenderOptions, writer: LimitedStringBuffer): ClientFuture[Unit] =
    generate(unit._internal, InternalRenderOptions(options), InternalShapeRenderOptions(shapeOptions), writer).asClient

  /** Asynchronously renders the syntax to a provided string buffer with limited capacity and returns it. */
  @JSExport
  def generateToWriter(unit: BaseUnit, writer: LimitedStringBuffer): ClientFuture[Unit] =
    generateToWriter(unit, RenderOptions(), ShapeRenderOptions(), writer)

  /** Asynchronously renders the syntax to a provided builder and returns it. */
  protected def genToBuilder[T](unit: BaseUnit, builder: DocBuilder[T]): ClientFuture[Unit] =
    genToBuilder(unit, RenderOptions(), ShapeRenderOptions(), builder)

  /** Asynchronously renders the syntax to a provided builder and returns it. */
  protected def genToBuilder[T](unit: BaseUnit, options: RenderOptions, builder: DocBuilder[T]): ClientFuture[Unit] =
    genToBuilder(unit, options, ShapeRenderOptions(), builder)

  /** Asynchronously renders the syntax to a provided builder and returns it. */
  protected def genToBuilder[T](unit: BaseUnit, shapeOptions: ShapeRenderOptions, builder: DocBuilder[T]): ClientFuture[Unit] =
    genToBuilder(unit, RenderOptions(), shapeOptions, builder)

  /** Asynchronously renders the syntax to a provided builder and returns it. */
  protected def genToBuilder[T](unit: BaseUnit, options: RenderOptions, shapeOptions: ShapeRenderOptions, builder: DocBuilder[T]): ClientFuture[Unit] =
    generate(unit._internal, InternalRenderOptions(options), InternalShapeRenderOptions(shapeOptions), builder).asClient

  /**
    * Generates the syntax text and stores it in the file pointed by the provided URL.
    * It must throw a UnsupportedOperation exception in platforms without support to write to the file system
    * (like the browser) or if a remote URL is provided.
    */
  private def generate(unit: InternalBaseUnit,
                       url: String,
                       options: InternalRenderOptions,
                       shapeOptions: InternalShapeRenderOptions): Future[Unit] = {
    new AMFSerializer(unit, mediaType, vendor, options, shapeOptions).renderToFile(platform, url)
  }

  private def generate(unit: InternalBaseUnit,
                       options: InternalRenderOptions,
                       shapeOptions: InternalShapeRenderOptions): Future[String] = {
    new AMFSerializer(unit, mediaType, vendor, options, shapeOptions).renderToString
  }

  private def generate[W: Output](unit: InternalBaseUnit,
                                  options: InternalRenderOptions,
                                  shapeOptions: InternalShapeRenderOptions,
                                  writer: W): Future[Unit] = {
    new AMFSerializer(unit, mediaType, vendor, options, shapeOptions).renderToWriter(writer)
  }

  private def generate[T](unit: InternalBaseUnit,
                          options: InternalRenderOptions,
                          shapeOptions: InternalShapeRenderOptions,
                          builder: DocBuilder[T]): Future[Unit] = {
    new AMFSerializer(unit, mediaType, vendor, options).renderToBuilder(builder)
  }
}
