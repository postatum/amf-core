package amf.core.model.domain

import amf.core.annotations.ScalarType
import amf.core.metamodel.Type.EncodedIri
import amf.core.metamodel.domain.DataNodeModel._
import amf.core.metamodel.domain.{ScalarNodeModel, _}
import amf.core.metamodel.{Field, Obj}
import amf.core.model.domain.ScalarNode.forDataType
import amf.core.model.domain.templates.Variable
import amf.core.model.{DataType, StrField}
import amf.core.parser.{Annotations, FieldEntry, Fields, Value}
import amf.core.resolution.VariableReplacer
import amf.core.utils._
import amf.core.vocabulary.Namespace.Data
import amf.core.vocabulary.{Namespace, ValueType}
import org.yaml.model.{YPart, YSequence}

import scala.collection.mutable

/**
  * Base class for all dynamic DataNodes
  */
abstract class DataNode(annotations: Annotations) extends DomainElement {

  def name: StrField = fields.field(Name)

  def withName(name: String): this.type = set(Name, name)

  override def adopted(parent: String, cycle: Seq[String] = Seq()): this.type = {
    if (Option(id).isEmpty) simpleAdoption(parent) else this
  }

  override def componentId: String =
    "/" + name.option().getOrElse("data-node").urlComponentEncoded

  /** Replace all raml variables (any name inside double chevrons -> '<<>>') with the provided values. */
  def replaceVariables(values: Set[Variable], keys: Seq[ElementTree])(reportError: String => Unit): DataNode

  def forceAdopted(parent: String): this.type = {

    def isEnum(id: String) = id.split("/").dropRight(1).last == "enum"
    // TODO: refactor this. Ids are okay in parsing stage but are lost on trait resolution in domain element merging.

    val adoptedId = parent + "/" + name
      .option()
      .map(_.urlComponentEncoded)
      .orNull
    val newId = Option(id) match {
      case Some(oldId: String) if oldId.endsWith("/included") =>
        adoptedId + "/included"
      case Some(oldId: String) if isEnum(oldId) =>
        adoptedId + "/enum"
      case _ => adoptedId
    }
    withId(newId)
  }

  override val fields: Fields = Fields()

  def copyNode(): DataNode
}

object DataNodeOps {

  /** Adopt entire data node hierarchy. */
  def adoptTree(id: String, node: DataNode): DataNode = {
    node.forceAdopted(id)
    node match {
      case array: ArrayNode =>
        array.members.foreach(adoptTree(array.id, _))
      case obj: ObjectNode =>
        obj
          .propertyFields()
          .map(obj.fields.field[DataNode])
          .foreach(e => adoptTree(obj.id, e))
      case _ =>
    }
    node
  }
}

/**
  * Data records, with a list of properties
  */
class ObjectNode(override val fields: Fields, val annotations: Annotations) extends DataNode(annotations) {

  def getFromKey(key: String): Option[DataNode] =
    fields
      .getValueAsOption(createField(key))
      .collectFirst({ case Value(e: DataNode, _) => e })

  def addProperty(propertyOrUri: String, objectValue: DataNode, annotations: Annotations = Annotations()): this.type = {
    val property = ensurePlainProperty(propertyOrUri)
    objectValue.set(DataNodeModel.Name, property)
    addPropertyByField(createField(property), objectValue, annotations)
    this
  }

  private def createField(property: String) = {
    Field(DataNodeModel, Namespace.Data + property.urlComponentEncoded, ModelDoc(ModelVocabularies.Data, property))
  }

  def addPropertyByField(f: Field, objectValue: DataNode, annotations: Annotations = Annotations()): this.type = {
    objectValue.adopted(this.id)
    set(f, objectValue, annotations)
    this
  }

  def propertyFields(): Iterable[Field] =
    fields
      .fields()
      .flatMap({
        case FieldEntry(field, _) if !ObjectNodeModel.fields.contains(field) =>
          Some(field)
        case _ => None
      })

  def allProperties(): Iterable[DataNode] = allPropertiesWithName().values

  def allPropertiesWithName(): Map[String, DataNode] =
    propertyFields().map(f => f.value.name.urlComponentDecoded -> fields[DataNode](f)).toMap

  protected def ensurePlainProperty(propertyOrUri: String): String =
    if (propertyOrUri.indexOf(Namespace.Data.base) == 0) {
      propertyOrUri.replace(Namespace.Data.base, "")
    } else {
      propertyOrUri
    }

  override val meta: Obj = new ObjectNodeDynamicModel()

  override def replaceVariables(values: Set[Variable], keys: Seq[ElementTree])(
      reportError: String => Unit): DataNode = {

    propertyFields().foreach { field =>
      val decodedKey = field.value.name.urlComponentDecoded
      val finalKey: String =
        if (decodedKey.endsWith("?"))
          decodedKey.substring(0, decodedKey.length - 1)
        else decodedKey
      val maybeTree = keys.find(_.key.equals(finalKey))

      fields.entry(field) match {
        case Some(FieldEntry(_, v)) =>
          val value = v.value
            .asInstanceOf[DataNode]
            .replaceVariables(values, maybeTree.map(_.subtrees).getOrElse(Nil))(
              if (decodedKey
                    .endsWith("?") && maybeTree.isEmpty) // TODO review this logic
                (_: String) => Unit
              else reportError) // if its an optional node, ignore the violation of the var not implement
          fields.removeField(field)
          addProperty(VariableReplacer.replaceVariablesInKey(decodedKey, values, reportError), value, v.annotations)
        case _ =>
      }
    }

    this
  }

  class ObjectNodeDynamicModel extends ObjectNodeModel {
    override val `type`: List[ValueType] = Data + "Object" :: DataNodeModel.`type`

    override def fields: List[Field] =
      propertyFields().toList ++ DataNodeModel.fields

    override def modelInstance: AmfObject = ObjectNode()

    override val doc: ModelDoc = ModelDoc(
      ModelVocabularies.Data,
      "ObjectNode",
      "Node that represents a dynamic object with records data structure"
    )
  }

  override def copyNode(): ObjectNode = {

    val cloned = new ObjectNode(fields.copy(), annotations.copy())

    if (id != null) cloned.withId(id)

    propertyFields().flatMap(f => fields.entry(f)).foreach { entry =>
      val value = entry.value
      cloned.set(entry.field, value.value.asInstanceOf[DataNode].copyNode(), value.annotations)
    }

    cloned
  }
}

object ObjectNode {

  val builderType: ValueType = Namespace.Data + "Object"

  def apply(): ObjectNode = apply(Annotations())

  def apply(ast: YPart): ObjectNode = apply(Annotations(ast))

  def apply(annotations: Annotations): ObjectNode =
    new ObjectNode(Fields(), annotations)

}

/**
  * Scalar values with associated data type
  */
class ScalarNode(override val fields: Fields, val annotations: Annotations) extends DataNode(annotations) {

  def withValue(v: String): this.type = withValue(v, Annotations())

  def withValue(v: String, ann: Annotations): this.type =
    set(ScalarNodeModel.Value, AmfScalar(v, ann))

  def withDataType(dataType: String): this.type = {
    set(ScalarNodeModel.DataType, forDataType(dataType))
  }

  def withDataType(dataType: String, ann: Annotations): this.type =
    set(ScalarNodeModel.DataType, AmfScalar(dataType, ann))

  def value: StrField = fields.field(ScalarNodeModel.Value)

  def dataType: StrField = fields.field(ScalarNodeModel.DataType)

  override def meta: Obj = ScalarNodeModel

  override def replaceVariables(values: Set[Variable], keys: Seq[ElementTree])(
      reportError: String => Unit): DataNode = {
    VariableReplacer.replaceNodeVariables(this, values, reportError)
  }
  override def copyNode(): DataNode =
    new ScalarNode(fields.copy(), annotations.copy()).withId(id)

}

object ScalarNode {
  def apply(): ScalarNode = apply("", None)

  def apply(annotations: Annotations): ScalarNode = apply("", None, annotations)

  def apply(value: String, dataType: Option[String]): ScalarNode =
    apply(value, dataType, Annotations())

  def apply(value: String, dataType: Option[String], ast: YPart): ScalarNode =
    apply(value, dataType, Annotations(ast))

  def apply(value: String, dataType: Option[String], annotations: Annotations): ScalarNode = {
    val scalar = new ScalarNode(Fields(), annotations)
    dataType.foreach(d => {
      annotations += ScalarType(d)
      scalar.withDataType(d)
    })
    scalar.set(ScalarNodeModel.Value, AmfScalar(value, annotations))
  }

  def forDataType(dataTypeUri: String): AmfScalar = dataTypeUri match {
    case DataType.String => string
    case DataType.Integer => integer
    case DataType.Number => number
    case DataType.Long => long
    case DataType.Double => double
    case DataType.Float => float
    case DataType.Decimal => decimal
    case DataType.Boolean => boolean
    case DataType.Date => date
    case DataType.Time => time
    case DataType.DateTime => dateTime
    case DataType.DateTimeOnly => dateTimeOnly
    case DataType.File => file
    case DataType.Byte => byte
    case DataType.Binary => base64Binary
    case DataType.Password => password
    case DataType.Any => anyType
    case DataType.AnyUri => anyURI
    case DataType.Nil => nil
    case _                    => AmfScalar(dataTypeUri, Annotations())
  }

  private val string = AmfScalar(DataType.String, Annotations.empty)
  private val integer = AmfScalar(DataType.Integer, Annotations.empty)
  private val number = AmfScalar(DataType.Number, Annotations.empty)
  private val long = AmfScalar(DataType.Long, Annotations.empty)
  private val double = AmfScalar(DataType.Double, Annotations.empty)
  private val float = AmfScalar(DataType.Float, Annotations.empty)
  private val decimal = AmfScalar(DataType.Decimal, Annotations.empty)
  private val boolean = AmfScalar(DataType.Boolean, Annotations.empty)
  private val date = AmfScalar(DataType.Date, Annotations.empty)
  private val time = AmfScalar(DataType.Time, Annotations.empty)
  private val dateTime = AmfScalar(DataType.DateTime, Annotations.empty)
  private val dateTimeOnly = AmfScalar(DataType.DateTimeOnly, Annotations.empty)
  private val file = AmfScalar(DataType.File, Annotations.empty)
  private val byte = AmfScalar(DataType.Byte, Annotations.empty)
  private val base64Binary = AmfScalar(DataType.Binary, Annotations.empty)
  private val password = AmfScalar(DataType.Password, Annotations.empty)
  private val anyType = AmfScalar(DataType.Any, Annotations.empty)
  private val anyURI = AmfScalar(DataType.AnyUri, Annotations.empty)
  private val nil = AmfScalar(DataType.Nil, Annotations.empty)
}

/**
  * Arrays of values
  */
class ArrayNode(override val fields: Fields, val annotations: Annotations) extends DataNode(annotations) {

  def members: Seq[DataNode] = fields.field(ArrayNodeModel.Member)

  def addMember(member: DataNode): Seq[DataNode] = {
    val newArray = members :+ member
    set(ArrayNodeModel.Member, AmfArray(newArray))
    newArray
  }

  def withMembers(members: Seq[DataNode]): this.type = {
    set(ArrayNodeModel.Member, AmfArray(members))
    this
  }

  override def replaceVariables(values: Set[Variable], keys: Seq[ElementTree])(
      reportError: String => Unit): DataNode = {
    val newMembers = members.map(_.replaceVariables(values, keys)(reportError))
    withMembers(newMembers)
  }

  def positionFields(): Seq[Field] = members.zipWithIndex.map {
    case (_, i) =>
      Field(EncodedIri, Namespace.Data + s"pos$i", ModelDoc(ModelVocabularies.Data, s"pos$i"))
  }

  override def meta: Obj = ArrayNodeModel

  override def copyNode(): this.type = {
    val cloned =
      new ArrayNode(fields.copy().filter(e => e._1 != ArrayNodeModel.Member), annotations.copy()).withId(id)

    if (id != null) cloned.withId(id)

    cloned.withMembers(members.map(_.copyNode()))

    cloned.asInstanceOf[this.type]
  }
}

object ArrayNode {

  val builderType: ValueType = Namespace.Data + "Array"

  def apply(): ArrayNode = apply(Annotations())

  def apply(ast: YSequence): ArrayNode = apply(Annotations(ast))

  def apply(annotations: Annotations): ArrayNode =
    new ArrayNode(Fields(), annotations)
}

/**
  * Dynamic node representing a link to another dynamic node
  * @param fields default fields for the dynamic node
  * @param annotations default annotations for the dynamic node
  */
class LinkNode(override val fields: Fields, val annotations: Annotations) extends DataNode(annotations) {

  def link: StrField  = fields.field(LinkNodeModel.Value)
  def alias: StrField = fields.field(LinkNodeModel.Alias)

  def withLink(link: String): this.type   = set(LinkNodeModel.Value, link)
  def withAlias(alias: String): this.type = set(LinkNodeModel.Alias, alias)

  override private[amf] def cloneElement(branch: mutable.Map[AmfObject, AmfObject]) = {
    val node = super.cloneElement(branch).asInstanceOf[LinkNode]
    linkedDomainElement.foreach(node.withLinkedDomainElement)
    node
  }

  var linkedDomainElement: Option[DomainElement] = None

  override def replaceVariables(values: Set[Variable], keys: Seq[ElementTree])(reportError: String => Unit): DataNode =
    this

  def withLinkedDomainElement(domainElement: DomainElement): LinkNode = {
    linkedDomainElement = Some(domainElement)
    this
  }

  override def meta: Obj = LinkNodeModel

  override def copyNode(): LinkNode = {
    val cloned = new LinkNode(fields.copy(), annotations.copy()).withId(id)

    cloned.linkedDomainElement = linkedDomainElement
    cloned
  }
}

object LinkNode {

  val builderType: ValueType = Namespace.Data + "Link"

  def apply(): LinkNode = apply(Annotations())

  def apply(annotations: Annotations): LinkNode = apply("", "", annotations)

  def apply(alias: String, value: String): LinkNode =
    apply(alias, value, Annotations())

  def apply(alias: String, value: String, annotations: Annotations): LinkNode = {
    val linkNode = new LinkNode(Fields(), annotations)
    linkNode.set(LinkNodeModel.Value, value)
    linkNode.set(LinkNodeModel.Alias, alias)
    linkNode
  }
}

case class ElementTree(key: String, subtrees: Seq[ElementTree])
