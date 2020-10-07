package amf.core.metamodel.domain

import amf.core.metamodel.Field
import amf.core.metamodel.Type.{Array, Bool, Iri, SortedArray, Str}
import amf.core.metamodel.domain.common.DescriptionField
import amf.core.metamodel.domain.extensions.{PropertyShapeModel, ShapeExtensionModel}
import amf.core.metamodel.domain.templates.KeyField
import amf.core.vocabulary.Namespace.{Core, Shacl, Shapes}
import amf.core.vocabulary.{Namespace, ValueType}

/**
  * Base class for all shapes. Shapes are Domain Entities that define constraints over parts of a data graph.
  * They can be used to define and enforce schemas for the data graph information through SHACL.
  * Shapes can be recursive and inherit from other shapes.
  */
trait ShapeModel extends DomainElementModel with LinkableElementModel with KeyField with DescriptionField {

  val Name = Field(Str, Shacl + "name", ModelDoc(ExternalModelVocabularies.Shacl, "name", "Name for a data shape"))

  val DisplayName =
    Field(Str, Core + "name", ModelDoc(ModelVocabularies.Core, "displayName", "Human readable name for the term"))

  val Default = Field(
    DataNodeModel,
    Shacl + "defaultValue",
    ModelDoc(ExternalModelVocabularies.Shacl, "defaultValue", "Default value parsed for a data shape property"))

  // TODO: change namespace
  val DefaultValueString = Field(
    Str,
    Shacl + "defaultValueStr",
    ModelDoc(ExternalModelVocabularies.Shacl,
             "defaultValueString",
             "Textual representation of the parsed default value for the shape property")
  )

  val Values = Field(
    SortedArray(DataNodeModel),
    Shacl + "in",
    ModelDoc(ExternalModelVocabularies.Shacl, "in", "Enumeration of possible values for a data shape property"))

  val Closure = Field(
    Array(Iri),
    Shapes + "closure",
    ModelDoc(ModelVocabularies.Shapes,
             "inheritanceClosure",
             "Transitive closure of data shapes this particular shape inherits structure from")
  )

  /**
    * Inheritance relationship between shapes. Introduces the idea that the constraints defined by this shape are a specialization of the constraints of the base shapes.
    * Graphs validating this shape should also validate all the constraints for the base shapes
    */
  val Inherits = Field(
    Array(ShapeModel),
    Shapes + "inherits",
    ModelDoc(ModelVocabularies.Shapes, "inherits", "Relationship of inheritance between data shapes"))

  // Logical constraints:

  val Or = Field(Array(ShapeModel),
                 Shacl + "or",
                 ModelDoc(ExternalModelVocabularies.Shacl, "or", "Logical or composition of data shapes"))

  val And = Field(Array(ShapeModel),
                  Shacl + "and",
                  ModelDoc(ExternalModelVocabularies.Shacl, "and", "Logical and composition of data shapes"))

  val Xone = Field(
    Array(ShapeModel),
    Shacl + "xone",
    ModelDoc(ExternalModelVocabularies.Shacl, "exclusiveOr", "Logical exclusive or composition of data shapes"))

  val Not = Field(ShapeModel,
                  Shacl + "not",
                  ModelDoc(ExternalModelVocabularies.Shacl, "not", "Logical not composition of data shapes"))

  val If = Field(ShapeModel,
                  Shacl + "if",
                  ModelDoc(ExternalModelVocabularies.Shacl, "if", "Condition for applying composition of data shapes"))

  val Then = Field(ShapeModel,
                  Shacl + "then",
                  ModelDoc(ExternalModelVocabularies.Shacl, "then", "Composition of data shape when if data shape is valid"))

  val Else = Field(ShapeModel,
                  Shacl + "else",
                  ModelDoc(ExternalModelVocabularies.Shacl, "else", "Composition of data shape when if data shape is invalid"))

  val ReadOnly =
    Field(Bool, Shapes + "readOnly", ModelDoc(ModelVocabularies.Shapes, "readOnly", "Read only property constraint"))

  val WriteOnly = Field(Bool,
    Shapes + "writeOnly",
    ModelDoc(ModelVocabularies.Shapes, "writeOnly", "Write only property constraint"))

  val Deprecated = Field(
    Bool,
    Shapes + "deprecated",
    ModelDoc(ModelVocabularies.Shapes, "deprecated", "Deprecated annotation for a property constraint"))

  override val key: Field = Name

  // RAML user-defined facets: definitions and values
  lazy val CustomShapePropertyDefinitions = Field(
    Array(PropertyShapeModel),
    Shapes + "customShapePropertyDefinitions",
    ModelDoc(ModelVocabularies.Shapes,
             "customShapePropertyDefinitions",
             "Custom constraint definitions added over a data shape")
  )
  lazy val CustomShapeProperties = Field(
    Array(ShapeExtensionModel),
    Shapes + "customShapeProperties",
    ModelDoc(ModelVocabularies.Shapes, "customShapeProperties", "Custom constraint values for a data shape")
  )
  //

}

object ShapeModel extends ShapeModel {

  override val fields: List[Field] = LinkableElementModel.fields ++ List(Name,
                                                                         DisplayName,
                                                                         Description,
                                                                         Default,
                                                                         Values,
                                                                         Inherits,
                                                                         DefaultValueString,
                                                                         Not,
                                                                         And,
                                                                         Or,
                                                                         Xone,
                                                                         Closure,
                                                                         If,
                                                                         Then,
                                                                         Else,
                                                                         ReadOnly,
                                                                         WriteOnly,
                                                                         Deprecated)

  override val `type`: List[ValueType] = List(Shacl + "Shape", Shapes + "Shape") ++ DomainElementModel.`type`

  override def modelInstance = throw new Exception("Shape is abstract and it cannot be instantiated by default")

  override val doc: ModelDoc = ModelDoc(
    ModelVocabularies.Shapes,
    "Shape",
    "Base class for all shapes. Shapes are Domain Entities that define constraints over parts of a data graph.\nThey can be used to define and enforce schemas for the data graph information through SHACL.\nShapes can be recursive and inherit from other shapes.",
    superClasses = Seq((Namespace.Shacl + "Shape").iri())
  )
}
