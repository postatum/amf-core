package amf.plugins.document.graph.parser

import org.yaml.model.{YMap, YMapEntry, YNode, YSequence, YType}
import amf.core.parser.YMapOps

case class JsonLdGraphContextParser(node: YNode, context: GraphContext = GraphContext()) {

  def parseMap(map: YMap): GraphContext = {

    map.entries.foreach { entry =>
      val key       = entry.key.as[String]
      val valueType = entry.value.tagType
      (key, valueType) match {
        case ("@base", YType.Str) => context.withBase(entry.value.as[String])
        case (term, YType.Str)    => parseSimpleTermEntry(entry, term)
        case (term, YType.Map)    => parseExpandedTermEntry(entry, term)
        case _                    => // Ignore
      }
    }
    context
  }

  private def parseExpandedTermEntry(entry: YMapEntry, term: String): Unit = {
    val termMap = entry.value.value.asInstanceOf[YMap]

    val id     = termMap.key("@id").map(_.value.as[String])
    val `type` = termMap.key("@type").map(_.value.as[String])

    context.withTerm(term, id, `type`)
  }
  private def parseSimpleTermEntry(entry: YMapEntry, term: String): Unit = {
    val namespace = entry.value.as[String]
    context.withTerm(term, namespace)
  }
  def parseRemoteContext(str: String): GraphContext = throw new NotImplementedError("Remote contexts are not supported")

  def parse(): GraphContext = {
    node.tagType match {
      case YType.Map => parseMap(node.value.asInstanceOf[YMap])
      case YType.Str => parseRemoteContext(node.as[String])
      case _         =>
        // throw error
        context
    }
  }

}
