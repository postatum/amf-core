package amf.core.parser

import org.yaml.model.YDocument

abstract class ParsedDocument {
  def comment: Option[String] = None
}

case class SyamlParsedDocument(document: YDocument, override val comment: Option[String] = None) extends ParsedDocument
