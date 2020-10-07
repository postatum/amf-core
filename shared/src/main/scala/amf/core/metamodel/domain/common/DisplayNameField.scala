package amf.core.metamodel.domain.common

import amf.core.metamodel.Field
import amf.core.metamodel.Type.Str
import amf.core.metamodel.domain.{ModelVocabularies, ModelDoc}
import amf.core.vocabulary.Namespace.Core

/**
  * DisplayName field.
  */
trait DisplayNameField {
  val DisplayName = Field(
    Str,
    Core + "displayName",
    ModelDoc(ModelVocabularies.Core, "displayName", "Human readable name for the term"))
}

object DisplayNameField extends DisplayNameField
