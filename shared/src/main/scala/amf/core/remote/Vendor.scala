package amf.core.remote

import scala.scalajs.js.annotation.JSExportTopLevel

/**
  * Created by pedro.colunga on 10/9/17.
  */
@JSExportTopLevel("core.Vendor")
object Vendor {
  def unapply(name: String): Option[Vendor] = {
    name match {
      case Raml10.name  => Some(Raml10)
      case Raml08.name  => Some(Raml08)
      case Raml.name    => Some(Raml) // todo remove later
      case Oas20.name   => Some(Oas20)
      case Oas30.name   => Some(Oas30)
      case Oas.name     => Some(Oas)
      case Amf.name     => Some(Amf)
      case Payload.name => Some(Payload)
      case _            => None
    }
  }

  val RAML: Vendor    = Raml
  val RAML08: Vendor  = Raml08
  val RAML10: Vendor  = Raml10
  val OAS: Vendor     = Oas
  val OAS20: Vendor   = Oas20
  val OAS30: Vendor   = Oas30
  val AMF: Vendor     = Amf
  val PAYLOAD: Vendor = Payload
}

sealed trait Vendor {
  val name: String
}

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

object Amf extends Vendor {
  override val name: String = "AMF Graph"
}

object Payload extends Vendor {
  override val name: String = "AMF Payload"
}

object JsonSchema extends Vendor {
  override val name: String = "JSON Schema"
}
