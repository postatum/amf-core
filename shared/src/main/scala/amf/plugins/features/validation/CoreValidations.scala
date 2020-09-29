package amf.plugins.features.validation

import amf._
import amf.core.validation.SeverityLevels._
import amf.core.validation.core.ValidationSpecification
import amf.core.validation.core.ValidationSpecification.CORE_VALIDATION
import amf.core.vocabulary.Namespace
import amf.core.vocabulary.Namespace.AmfCore

// noinspection TypeAnnotation
object CoreValidations extends Validations {
  override val specification: String = CORE_VALIDATION
  override val namespace: Namespace  = AmfCore

  val CycleReferenceError = validation(
    "cycle-reference",
    "Cycle in references"
  )

  val InvalidCrossSpec = validation(
    "invalid-cross-spec",
    "Cross spec file usage is not allowed"
  )

  val UnresolvedReference = validation(
    "unresolved-reference",
    "Unresolved reference"
  )

  val UriSyntaxError = validation(
    "uri-syntax-error",
    "invalid uri syntax"
  )

  val InvalidFragmentRef = validation(
    "invalid-fragment-ref",
    "References with # in RAML are not allowed"
  )

  val DeclarationNotFound = validation(
    "declaration-not-found",
    "Declaration not found"
  )

  val SyamlError = validation(
    "syaml-error",
    "Syaml error"
  )

  val SyamlWarning = validation(
    "syaml-warning",
    "Syaml warning"
  )

  val ExpectedModule = validation(
    "expected-module",
    "Expected Module"
  )

  val InvalidInclude = validation(
    "invalid-include",
    "Invalid !include value"
  )

  val UnableToParseNode = validation(
    "parse-node-fail",
    "JsonLD @types failed to parse in node"
  )

  val UnableToConvertToScalar = validation(
    "unable-to-convert-scalar",
    "Unable to convert scalar"
  )

  val UnableToParseRdfDocument = validation(
    "parse-rdf-document-fail",
    "Unable to parse rdf document"
  )

  val NodeNotFound = validation(
    "node-not-found",
    "Builder for model not found"
  )

  val NotLinkable = validation(
    "not-linkable",
    "Only linkable elements can be linked"
  )

  val UnableToParseDocument = validation(
    "parse-document-fail",
    "Unable to parse document"
  )

  val MissingIdInNode = validation(
    "missing-id-in-node",
    "Missing @id in json-ld node"
  )

  val MissingTypeInNode = validation(
    "missing-type-in-node",
    "Missing @type in json-ld node"
  )

  // Used in resolution
  val RecursiveShapeSpecification = validation(
    "recursive-shape",
    "Recursive shape",
    Some("Recursive type"),
    Some("Recursive schema")
  )

  // Used in resolution
  val ResolutionValidation = validation(
    "resolution-validation",
    "Default resolution validation"
  )

  val UnhandledDomainElement = validation(
    "unhandled-element",
    "Unhandled domain element for given vendor"
  )

  override val levels: Map[String, Map[ProfileName, String]] = Map(
    SyamlWarning.id -> all(WARNING),
    RecursiveShapeSpecification.id -> Map(
      RamlProfile   -> VIOLATION,
      Raml10Profile -> VIOLATION,
      Raml08Profile -> VIOLATION,
      OasProfile    -> VIOLATION,
      Oas20Profile  -> VIOLATION,
      Oas30Profile  -> VIOLATION,
      AmfProfile    -> INFO
    )
  )

  override val validations: List[ValidationSpecification] = List(
    CycleReferenceError,
    NotLinkable,
    UnresolvedReference,
    SyamlError,
    SyamlWarning,
    NodeNotFound,
    UnableToParseDocument,
    UnableToParseNode,
    ExpectedModule,
    MissingIdInNode,
    MissingTypeInNode,
    UriSyntaxError,
    UnableToParseRdfDocument,
    DeclarationNotFound,
    InvalidInclude,
    InvalidCrossSpec,
    InvalidFragmentRef,
    RecursiveShapeSpecification,
    ResolutionValidation,
    UnhandledDomainElement
  )
}
