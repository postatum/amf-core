package amf.core.metamodel.domain

import amf.core.metamodel.Field
import amf.core.metamodel.Type.{Array, Iri, Str}
import amf.core.metamodel.domain.common.NameFieldSchema
import amf.core.model.domain._
import amf.core.vocabulary.Namespace.Data
import amf.core.vocabulary.{Namespace, ValueType}

/**
  * Data Model to parse any generic data structure defined
  * by recursive records with arrays and scalar values (think of JSON or RAML)
  * into a RDF graph.
  *
  * This can be used to parse value of annotations, payloads or
  * examples
  */
object DataNodeModel extends DomainElementModel with NameFieldSchema {

  // We set this so it can be re-used in the definition of the dynamic types
  override val fields: List[Field]     = List(Name) ++ DomainElementModel.fields
  override val `type`: List[ValueType] = Data + "Node" :: DomainElementModel.`type`

  override def modelInstance =
    throw new Exception("DataNode is an abstract class and it cannot be instantiated directly")

  override val doc: ModelDoc = ModelDoc(
    ModelVocabularies.Data,
    "DataNode",
    "Base class for all data nodes parsed from the data structure"
  )
}

trait ObjectNodeModel extends DomainElementModel {

  override def fields: List[Field]      = DataNodeModel.fields
  override val `type`: List[ValueType]  = Data + "Object" :: DataNodeModel.`type`
  override def modelInstance: AmfObject = ObjectNode()

  override val doc: ModelDoc = ModelDoc(
    ModelVocabularies.Data,
    "ObjectNode",
    "Node that represents a dynamic object with records data structure"
  )
}

object ObjectNodeModel extends ObjectNodeModel

object ScalarNodeModel extends DomainElementModel {

  val Value =
    Field(Str, Namespace.Data + "value", ModelDoc(ModelVocabularies.Data, "value", "value for an scalar dynamic node"))

  val DataType =
    Field(Iri, Namespace.Shacl + "datatype", ModelDoc(ModelVocabularies.Data, "dataType", "Data type of value for an scalar dynamic node"))

  override def fields: List[Field]      = Value :: DataType :: DataNodeModel.fields
  override val `type`: List[ValueType]  = Data + "Scalar" :: DataNodeModel.`type`
  override def modelInstance: AmfObject = ScalarNode()

  override val doc: ModelDoc = ModelDoc(
    ModelVocabularies.Data,
    "ScalarNode",
    "Node that represents a dynamic scalar value data structure"
  )
}

object ArrayNodeModel extends DomainElementModel {

  val Member =
    Field(Array(DataNodeModel), Namespace.Rdfs + "member", ModelDoc(ExternalModelVocabularies.Rdf, "member", ""))

  override val fields: List[Field]      = Member :: DataNodeModel.fields
  override val `type`: List[ValueType]  = Data + "Array" :: Namespace.Rdf + "Seq" :: DataNodeModel.`type`
  override def modelInstance: AmfObject = ArrayNode()

  override val doc: ModelDoc = ModelDoc(
    ModelVocabularies.Data,
    "ArrayNode",
    "Node that represents a dynamic array data structure"
  )
}

object LinkNodeModel extends DomainElementModel {

  val Value: Field = Field(Str, Namespace.Data + "value", ModelDoc(ModelVocabularies.Data, "value"))
  val Alias: Field = Field(Str, Namespace.Data + "alias", ModelDoc(ModelVocabularies.Data, "alias"))

  override def fields: List[Field]      = List(Value) ++ DataNodeModel.fields
  override val `type`: List[ValueType]  = Data + "Link" :: DataNodeModel.`type`
  override def modelInstance: AmfObject = LinkNode()

  override val doc: ModelDoc = ModelDoc(
    ModelVocabularies.Data,
    "LinkNode",
    "Node that represents a dynamic link in a data structure"
  )
}
