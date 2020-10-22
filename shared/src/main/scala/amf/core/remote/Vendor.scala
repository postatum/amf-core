package amf.core.remote

import scala.scalajs.js.annotation.{JSExport, JSExportAll, JSExportTopLevel}

@JSExportAll
@JSExportTopLevel("core.Vendor")
object Vendor {
  def unapply(name: String): Option[Vendor] = {
    name match {
      case Raml10.name     => Some(Raml10)
      case Raml08.name     => Some(Raml08)
      case Raml.name       => Some(Raml)
      case Oas20.name      => Some(Oas20)
      case Oas30.name      => Some(Oas30)
      case Oas.name        => Some(Oas)
      case AsyncApi.name   => Some(AsyncApi)
      case AsyncApi20.name => Some(AsyncApi20)
      case Amf.name        => Some(Amf)
      case Payload.name    => Some(Payload)
      case Aml.name        => Some(Aml)
      case _               => None
    }
  }

  @JSExport("apply")
  def apply(name: String): Vendor = name match {
    case Vendor(vendor) => vendor
    case _              => new UnknowVendor(name)
  }

  val RAML: Vendor    = Raml
  val RAML08: Vendor  = Raml08
  val RAML10: Vendor  = Raml10
  val OAS: Vendor     = Oas
  val OAS20: Vendor   = Oas20
  val OAS30: Vendor   = Oas30
  val ASYNC: Vendor   = AsyncApi
  val ASYNC20: Vendor = AsyncApi20
  val AMF: Vendor     = Amf
  val PAYLOAD: Vendor = Payload
  val AML: Vendor     = Aml
}

sealed trait Vendor {
  val name: String

  def isRaml: Boolean  = this == Raml || this == Raml10 || this == Raml08
  def isOas: Boolean   = this == Oas || this == Oas20 || this == Oas30
  def isAsync: Boolean = this == AsyncApi || this == AsyncApi20
}

class UnknowVendor(override val name: String) extends Vendor

trait Raml extends Vendor {
  def version: String

  override val name: String = ("RAML " + version).trim

  override def toString: String = name.trim
}

trait Oas extends Vendor {
  def version: String

  override val name: String = ("OAS " + version).trim

  override def toString: String = name.trim
}

trait Async extends Vendor {
  def version: String

  override val name: String = ("ASYNC " + version).trim

  override def toString: String = name.trim
}

object Aml extends Vendor {

  override val name: String = "AML 1.0"

  override def toString: String = name.trim
}

object Oas extends Oas {
  override def version: String = ""
}

object Oas20 extends Oas {
  override def version: String = "2.0"
}

object Oas30 extends Oas {
  override def version: String = "3.0"
}

object Raml extends Raml {
  override def version: String = ""
}

object Raml08 extends Raml {
  override def version: String = "0.8"
}

object Raml10 extends Raml {
  override def version: String = "1.0"
}

object AsyncApi extends Async {
  override def version: String = ""
}

object AsyncApi20 extends Async {
  override def version: String = "2.0"
}

object Amf extends Vendor {
  override val name: String = "AMF Graph"
}

object Payload extends Vendor {
  override val name: String = "AMF Payload"
}

object JsonSchema extends Vendor {
  override val name: String = "JSON Schema"

  override def toString: String = name.trim
}
