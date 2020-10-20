package amf.core.model

import amf.core.vocabulary.Namespace.{Shapes, Xsd}

object DataType {

  // Repeated logic with XsdTypes
  val String: String       = Xsd.base + "string"
  val Integer: String      = Xsd.base + "integer"
  val Number: String       = Shapes.base + "number"
  val Long: String         = Xsd.base + "long"
  val Double: String       = Xsd.base + "double"
  val Float: String        = Xsd.base + "float"
  val Decimal: String      = Xsd.base + "decimal"
  val Boolean: String      = Xsd.base + "boolean"
  val Date: String         = Xsd.base + "date"
  val Time: String         = Xsd.base + "time"
  val DateTime: String     = Xsd.base + "dateTime"
  val DateTimeOnly: String = Shapes.base + "dateTimeOnly"
  val Duration: String     = Xsd.base + "duration"
  val File: String         = Shapes.base + "file"
  val Byte: String         = Xsd.base + "byte"
  val Binary: String       = Xsd.base + "base64Binary"
  val Password: String     = Shapes.base + "password"
  val Any: String          = Xsd.base + "anyType"
  val AnyUri: String       = Xsd.base + "anyURI"
  val Link: String         = Shapes.base + "link"
  val Nil: String          = Xsd.base + "nil"

  /** Return dataType qualified with Xsd namespace. */
  def apply(dataType: String): String = dataType match {
    case "string"                         => String
    case "integer"                        => Integer
    case "number"                         => Number
    case "long"                           => Long
    case "double"                         => Double
    case "float"                          => Float
    case "decimal"                        => Decimal
    case "boolean"                        => Boolean
    case "date" | "date-only"             => Date
    case "time" | "time-only"             => Time
    case "dateTime" | "datetime"          => DateTime
    case "dateTimeOnly" | "datetime-only" => DateTimeOnly
    case "file"                           => File
    case "byte"                           => Byte
    case "base64Binary"                   => Binary
    case "password"                       => Password
    case "anyType" | "any"                => Any
    case "anyUri" | "uri"                 => AnyUri
    case "nil"                            => Nil
    case _                                => Xsd.base + dataType
  }
}
