package amf.plugins.document.graph.parser

import amf.core.metamodel.{Field, Obj, Type}
import amf.core.metamodel.Type._
import amf.core.metamodel.document.SourceMapModel
import amf.core.metamodel.domain.DomainElementModel
import amf.core.model.document.SourceMap
import amf.core.model.domain.{AmfElement, AmfScalar, Annotation}
import amf.core.parser.{Annotations, _}
import amf.core.vocabulary.{Namespace, ValueType}
import amf.core.vocabulary.Namespace.SourceMaps
import amf.plugins.features.validation.CoreValidations.{MissingIdInNode, MissingTypeInNode, namespace}
import org.mulesoft.common.time.SimpleDateTime
import org.yaml.convert.YRead.SeqNodeYRead
import org.yaml.model._
import amf.core.model.DataType

import scala.collection.{immutable, mutable}

trait GraphParserHelpers extends GraphContextHelper {

  protected def float(node: YNode): AmfScalar = {
    val value = node.tagType match {
      case YType.Map =>
        node.as[YMap].entries.find(_.key.as[String] == "@value") match {
          case Some(entry) =>
            entry.value.as[YScalar].text.toDouble
          case _ => node.as[YScalar].text.toDouble
        }
      case _ => node.as[YScalar].text.toDouble
    }
    AmfScalar(value)
  }

  protected def str(node: YNode): AmfScalar = {
    val value = node.tagType match {
      case YType.Map =>
        node.as[YMap].entries.find(_.key.as[String] == "@value") match {
          case Some(entry) => entry.value.as[YScalar].text
          case _           => node.as[YScalar].text
        }
      case _ => node.as[YScalar].text
    }
    AmfScalar(value)
  }

  protected def bool(node: YNode): AmfScalar = {
    val value = node.tagType match {
      case YType.Map =>
        node.as[YMap].entries.find(_.key.as[String] == "@value") match {
          case Some(entry) => entry.value.as[YScalar].text.toBoolean
          case _           => node.as[YScalar].text.toBoolean
        }
      case _ => node.as[YScalar].text.toBoolean
    }
    AmfScalar(value)
  }

  protected def int(node: YNode): AmfScalar = {
    val value = node.tagType match {
      case YType.Map =>
        node.as[YMap].entries.find(_.key.as[String] == "@value") match {
          case Some(entry) => entry.value.as[YScalar].text.toInt
          case _           => node.as[YScalar].text.toInt
        }
      case _ => node.as[YScalar].text.toInt
    }
    AmfScalar(value)
  }

  protected def double(node: YNode): AmfScalar = {
    val value = node.tagType match {
      case YType.Map =>
        node.as[YMap].entries.find(_.key.as[String] == "@value") match {
          case Some(entry) => entry.value.as[YScalar].text.toDouble
          case _           => node.as[YScalar].text.toDouble
        }
      case _ => node.as[YScalar].text.toDouble
    }
    AmfScalar(value)
  }

  protected def date(node: YNode): AmfScalar = {
    val value = node.tagType match {
      case YType.Map =>
        node.as[YMap].entries.find(_.key.as[String] == "@value") match {
          case Some(entry) =>
            SimpleDateTime.parse(entry.value.as[YScalar].text).right.get
          case _ => SimpleDateTime.parse(node.as[YScalar].text).right.get
        }
      case _ => SimpleDateTime.parse(node.as[YScalar].text).right.get
    }
    AmfScalar(value)
  }

  protected def any(node: YNode)(implicit ctx: GraphParserContext): AmfScalar = {
    node.tagType match {
      case YType.Map =>
        val nodeValue =
          node.as[YMap].entries.find(_.key.as[String] == "@value") match {
            case Some(entry) => entry.value.as[YScalar].text
            case _           => node.as[YScalar].text
          }
        node.as[YMap].entries.find(_.key.as[String] == "@type") match {
          case Some(typeEntry) =>
            val typeUri     = typeEntry.value.as[YScalar].text
            val expandedUri = expandUriFromContext(typeUri)
            expandedUri match {
              case s: String if s == DataType.Boolean =>
                AmfScalar(nodeValue.toBoolean)
              case s: String if s == DataType.Integer => AmfScalar(nodeValue.toInt)
              case s: String if s == DataType.Float   => AmfScalar(nodeValue.toFloat)
              case s: String if s == DataType.Double  => AmfScalar(nodeValue.toDouble)
              case s: String if s == DataType.DateTime =>
                AmfScalar(SimpleDateTime.parse(nodeValue).right.get)
              case s: String if s == DataType.Date =>
                AmfScalar(SimpleDateTime.parse(nodeValue).right.get)
              case _ => AmfScalar(nodeValue)
            }
          case _ => AmfScalar(nodeValue)
        }
      case _ => AmfScalar(node.as[YScalar].text)
    }
  }

  def defineField(field: Field)(ctx: GraphParserContext): Option[TermDefinition] = {
    ctx.graphContext
      .definitions()
      .find {
        case (term, _) => equal(term, field.value.iri())(ctx.graphContext)
      }
      .map {
        case (_, definition) => definition
      }
  }

  def assertFieldTypeWithContext(field: Field)(ctx: GraphParserContext): Boolean = {
    val contextDefinition = defineField(field)(ctx)
    contextDefinition match {
      case Some(definition: ExpandedTermDefinition) =>
        assertFieldTypeWithDefinition(field, definition)(ctx)
      case _ => true
    }
  }

  private def assertFieldTypeWithDefinition(field: Field, definition: ExpandedTermDefinition)(
      ctx: GraphParserContext) = {
    definition.`type`.forall { typeFromCtxDefinition =>
      val fieldTypes: immutable.Seq[ValueType] = field.`type`.`type`
      fieldTypes.exists(fieldType => equal(fieldType.iri(), typeFromCtxDefinition)(ctx.graphContext))
    }
  }

  protected def nodeIsOfType(node: YNode, obj: Obj)(implicit ctx: GraphParserContext): Boolean = {
    node.value match {
      case map: YMap =>
        map.key("@type").exists { entry =>
          val types = entry.value.as[YSequence].nodes.flatMap(_.asScalar)
          types.exists(`type` => {
            val typeIri = expandUriFromContext(`type`.text)
            obj.`type`.map(_.iri()).contains(typeIri)
          })
        }
      case _ => false
    }
  }

  private def parseSourceNode(map: YMap)(implicit ctx: GraphParserContext): SourceMap = {
    val result = SourceMap()
    map.entries.foreach(entry => {
      entry.key.toOption[YScalar].map(value => expandUriFromContext(value.text)).foreach {
        case AnnotationName(annotation) =>
          val consumer = result.annotation(annotation)
          entry.value
            .as[Seq[YNode]]
            .foreach(e => {
              contentOfNode(e) foreach { element =>
                val k = element.key(compactUriFromContext(SourceMapModel.Element.value.iri())).get
                val v = element.key(compactUriFromContext(SourceMapModel.Value.value.iri())).get
                consumer(value(SourceMapModel.Element.`type`, k.value).as[YScalar].text,
                         value(SourceMapModel.Value.`type`, v.value).as[YScalar].text)
              }
            })
        case _ => // Unknown annotation identifier
      }
    })
    result
  }

  def asIris(ns: Namespace, elements: Seq[String]): Seq[ValueType] = elements.map(element => ns + element)

  // declared so they can be referenced from the retrieveType* functions
  val amlDocumentIris: Seq[ValueType] =
    asIris(
        Namespace.Meta,
        Seq("DialectInstance",
            "DialectInstanceFragment",
            "DialectInstanceLibrary",
            "DialectInstancePatch",
            "DialectLibrary",
            "DialectFragment",
            "Dialect",
            "Vocabulary")
    )

  val coreDocumentIris: Seq[ValueType] =
    asIris(Namespace.Document, Seq("Document", "Fragment", "Module", "Unit"))

  val documentIris: Seq[ValueType] = amlDocumentIris ++ coreDocumentIris

  /**
    * Returns a list a sequence of type from a YMap defined in the @type entry
    * @param map ymap input
    * @param id some id to throw an error if type retrieval fails
    * @param ctx graph parsing context
    * @return
    */
  protected def ts(map: YMap, id: String)(implicit ctx: GraphParserContext): Seq[String] = {
    val documentExpandedIris: Seq[String] = coreDocumentIris.map(docElement => docElement.iri())
    val documentCompactIris               = documentExpandedIris.map(compactUriFromContext(_))

    val documentTypesSet: Set[String] = (documentExpandedIris ++ documentCompactIris).toSet

    map.key("@type") match {
      case Some(entry) =>
        val allTypes         = entry.value.toOption[Seq[YNode]].getOrElse(Nil).flatMap(v => v.toOption[YScalar].map(_.text))
        val nonDocumentTypes = allTypes.filter(t => !documentTypesSet.contains(t))
        val documentTypes    = allTypes.filter(t => documentTypesSet.contains(t)).sorted // we just use the fact that lexical order is correct
        nonDocumentTypes ++ documentTypes

      case _ =>
        ctx.eh.violation(MissingTypeInNode, id, s"No @type declaration on node $map", map) // todo : review with pedro
        Nil
    }
  }

  protected def retrieveId(map: YMap, ctx: ParserContext): Option[String] = {
    map.key("@id") match {
      case Some(entry) => Some(entry.value.as[YScalar].text)
      case _ =>
        ctx.eh.violation(MissingIdInNode, "", s"No @id declaration on node $map", map)
        None
    }
  }

  protected def contentOfNode(n: YNode): Option[YMap] = n.toOption[YMap]

  protected def retrieveSources(id: String, map: YMap)(implicit ctx: GraphParserContext): SourceMap = {
    map
      .key(compactUriFromContext(DomainElementModel.Sources.value.iri()))
      .flatMap { entry =>
        val srcNode = value(SourceMapModel, entry.value)
        contentOfNode(srcNode).map(parseSourceNode(_))
      }
      .getOrElse(SourceMap.empty)
  }

  protected def value(t: Type, node: YNode): YNode = {
    node.tagType match {
      case YType.Seq =>
        t match {
          case Array(_) => node
          case _        => value(t, node.as[Seq[YNode]].head)
        }
      case YType.Map =>
        val m: YMap = node.as[YMap]
        t match {
          case Iri                                       => m.key("@id").get.value
          case Str | RegExp | Bool | Type.Int | Type.Any => m.key("@value").get.value
          case _                                         => node
        }
      case _ => node
    }
  }

  protected object AnnotationName {
    def unapply(uri: String): Option[String] = uri match {
      case url if url.startsWith(SourceMaps.base) => Some(url.substring(url.indexOf("#") + 1))
      case _                                      => None
    }
  }

  protected def annotations(nodes: Map[String, AmfElement], sources: SourceMap, key: String): Annotations = {
    val result = Annotations()

    if (sources.nonEmpty) {
      sources.annotations.foreach {
        case (annotation, values: mutable.Map[String, String]) =>
          annotation match {
            case Annotation(deserialize) if values.contains(key) =>
              deserialize(values(key), nodes).foreach(result += _)
            case _ =>
          }
      }
    }

    result
  }
}

abstract class GraphContextHelper extends GraphContextOperations {

  protected def expandUriFromContext(iri: String)(implicit ctx: GraphParserContext): String = {
    expand(iri)(ctx.graphContext)
  }

  protected def compactUriFromContext(iri: String)(implicit ctx: GraphParserContext): String = {
    compact(iri)(ctx.graphContext)
  }

  protected def transformIdFromContext(id: String)(implicit ctx: GraphParserContext): String = {
    val prefixOption = ctx.graphContext.base.map { base =>
      if (id.startsWith("./")) base.parent.iri + "/"
      else base.iri
    }
    val prefix = prefixOption.getOrElse("")

    s"$prefix$id"
  }

}
