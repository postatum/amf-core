package amf.core.metamodel.domain.extensions

import amf.Core
import amf.core.metamodel.Field
import amf.core.metamodel.Type.Str
import amf.core.metamodel.domain.templates.KeyField
import amf.core.metamodel.domain.{DataNodeModel, DomainElementModel, ModelDoc, ModelVocabularies}
import amf.core.model.domain.extensions.DomainExtension
import amf.core.vocabulary.Namespace.{ApiContract, Document}
import amf.core.vocabulary.{Namespace, ValueType}

/**
  * Extension to the model being parsed from RAML annotation or OpenAPI extensions.
  * They must be a DomainPropertySchema (only in RAML) defining them.
  * The DomainPropertySchema might have an associated Data Shape that must validate
  * the extension nested graph.
  *
  * They are parsed as RDF graphs using a default transformation from a set of nested
  * records into RDF
  */
trait DomainExtensionModel extends DomainElementModel with KeyField {

  val Name = Field(Str, Namespace.Core + "extensionName", ModelDoc(ModelVocabularies.Core, "name", "Name of an extension entity"))
  val DefinedBy = Field(CustomDomainPropertyModel,
                        Document + "definedBy",
                        ModelDoc(ModelVocabularies.AmlDoc, "definedBy", "Definition for the extended entity"))
  val Extension = Field(DataNodeModel,
                        Document + "extension",
                        ModelDoc(ModelVocabularies.AmlDoc, "extension", "Data structure associated to the extension"))
  val Element =
    Field(Str, Document + "element", ModelDoc(ModelVocabularies.AmlDoc, "element", "Element being extended"))

  override val key: Field = Name

  override def fields: List[Field] = List(Name, DefinedBy, Extension, Element) ++ DomainElementModel.fields

  override val `type`: List[ValueType] = ApiContract + "DomainExtension" :: DomainElementModel.`type`
}

object DomainExtensionModel extends DomainExtensionModel {
  override def modelInstance = DomainExtension()

  override val doc: ModelDoc = ModelDoc(
    ModelVocabularies.ApiContract,
    "DomainExtension",
    "Extension to the model being parsed from RAML annotation or OpenAPI extensions\nThey must be a DomainPropertySchema (only in RAML) defining them.\nThe DomainPropertySchema might have an associated Data Shape that must validate the extension nested graph.\nThey are parsed as RDF graphs using a default transformation from a set of nested records into RDF."
  )
}
