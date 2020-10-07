package amf.core.metamodel.domain

import amf.core.metamodel.Type.{Iri, Str}
import amf.core.metamodel.{Field, Obj}
import amf.core.vocabulary.Namespace.{Document, Shacl}
import amf.core.vocabulary.{Namespace, ValueType}

trait ExternalSourceElementModel extends Obj {
  val Raw = Field(Str,
                  Document + "raw",
                  ModelDoc(ModelVocabularies.AmlDoc,
                           "raw",
                           "Raw textual information that cannot be processed for the current model semantics."))
  val ReferenceId = Field(
    Iri,
    Namespace.Document + "reference-id",
    ModelDoc(ModelVocabularies.AmlDoc, "referenceId", "Internal identifier for an inlined fragment"))
  val Location = Field(Str,
                       Document + "location",
                       ModelDoc(ModelVocabularies.AmlDoc, "location", "Location of an inlined fragment"))

}

object ExternalSourceElementModel extends ExternalSourceElementModel {

  override val fields                  = List(Raw, ReferenceId, Location)
  override val `type`: List[ValueType] = List(Namespace.Document + "ExternalSource")

  override val doc: ModelDoc = ModelDoc(
    ModelVocabularies.AmlDoc,
    "ExternalSourceElement",
    "Inlined fragment of information"
  )
}
