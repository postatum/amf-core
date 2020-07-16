package amf.core.validation.core

import amf.core.rdf.RdfModel
import amf.core.validation.core.ValidationSpecification.PARSER_SIDE_VALIDATION
import amf.core.validation.model.PropertyPath
import amf.core.vocabulary.Namespace
import org.yaml.model.YDocument.EntryBuilder

case class FunctionConstraintParameter(path: String, datatype: String)

case class FunctionConstraint(message: Option[String],
                              code: Option[String] = None,
                              libraries: Seq[String] = Seq(),
                              functionName: Option[String] = None,
                              parameters: Seq[FunctionConstraintParameter] = Seq(),
                              internalFunction: Option[String] = None) {

  def constraintId(validationId: String)  = s"${validationId}Constraint"
  def validatorId(validationId: String)   = s"${validationId}Validator"
  def validatorPath(validationId: String) = s"${validationId}Path"
  def validatorArgument(validationId: String): String = {
    "$" + validatorPath(validationId)
      .split("#")
      .last
      .replace("-", "_")
      .replace(".", "_")
  }
  def computeFunctionName(validationId: String): String = functionName match {
    case Some(fnName) => fnName
    case _ =>
      val localName = validationId.split("/").last.split("#").last
      s"${localName.replace("-", "_").replace(".", "_")}FnName"
  }
}

case class NodeConstraint(constraint: String, value: String)

case class PropertyConstraint(ramlPropertyId: String,
                              name: String,
                              // storing the constraint over a property path
                              path: Option[PropertyPath] = None,
                              // shacl:message
                              message: Option[String] = None,
                              pattern: Option[String] = None,
                              patternedProperty: Option[String] = None,
                              maxCount: Option[String] = None,
                              minCount: Option[String] = None,
                              minLength: Option[String] = None,
                              maxLength: Option[String] = None,
                              minExclusive: Option[String] = None,
                              maxExclusive: Option[String] = None,
                              minInclusive: Option[String] = None,
                              maxInclusive: Option[String] = None,
                              multipleOf: Option[String] = None,
                              /**
                                * shacl:node
                                * Objects of this property must conform to the
                                * provided node shape
                                */
                              node: Option[String] = None,
                              datatype: Option[String] = None,
                              // format: Option[String] = None,
                              /**
                                * shacl:class
                                * Objects of this property must have this class
                                */
                              `class`: Seq[String] = Seq(),
                              in: Seq[String] = Seq.empty,
                              custom: Option[(EntryBuilder, String) => Unit] = None,
                              customRdf: Option[(RdfModel, String) => Any] = None,
                              /**
                               * for qualified constraints
                               */
                              atLeast: Option[(Int, String)] = None,
                              atMost: Option[(Int, String)] = None,
                              value: Option[String] = None,
                              // Property comparisons
                              equalToProperty: Option[String] = None,
                              disjointWithProperty: Option[String] = None,
                              lessThanProperty: Option[String] = None,
                              lessThanOrEqualsToProperty: Option[String] = None,
                             )

case class QueryConstraint(
                            prefixes: Map[String,String],
                            query: String
                          )

case class ValidationSpecification(name: String,
                                   // shacl:message
                                   message: String,
                                   ramlMessage: Option[String] = None,
                                   oasMessage: Option[String] = None,
                                   /**
                                     * shacl:targetNode
                                     * URIs of the nodes in the graph that will be
                                     * targeted by this shape
                                     */
                                   targetInstance: Seq[String] = Seq.empty,
                                   /**
                                     * shacl:targetClass
                                     * Nodes with these classes will be targeted
                                     * by this shape
                                     */
                                   targetClass: Seq[String] = Seq.empty,
                                   /**
                                     * shacl:targetObjectsOf
                                     *
                                     * Nodes that are object of the properties in
                                     * this array will be targeted by this shape
                                     */
                                   targetObject: Seq[String] = Seq.empty,
                                   /**
                                     * Union of constraints passed as URIs
                                     * to the contraints in the union
                                     */
                                   unionConstraints: Seq[String] = Seq.empty,
                                   // Logical constraints here, or contraints are collected in the union above
                                   andConstraints: Seq[String] = Seq.empty,
                                   xoneConstraints: Seq[String] = Seq.empty,
                                   notConstraint: Option[String] = None,
                                   /**
                                     * shacl:property
                                     * Property constraints for the node
                                     */
                                   propertyConstraints: Seq[PropertyConstraint] = Seq.empty,
                                   nodeConstraints: Seq[NodeConstraint] = Seq.empty,
                                   closed: Option[Boolean] = None,
                                   functionConstraint: Option[FunctionConstraint] = None,
                                   custom: Option[(EntryBuilder, String) => Unit] = None,
                                  /*
                                   * Nested validations
                                   */
                                   nested: Option[String] = None,
                                  /*
                                   * transition from JS functions to complex ones
                                   */
                                   replacesFunctionConstraint: Option[String] = None,
                                  /*
                                   * Query validation
                                   */
                                   query: Option[QueryConstraint] = None
                                  ) {

  val id: String = {
    if (name.startsWith("http://") || name.startsWith("https://") || name.startsWith("file:")) {
      name
    } else {
      Namespace.expand(name).iri() match {
        case s if s.startsWith("http://") || s.startsWith("https://") || s.startsWith("file:") => s
        case s                                                                                 => (Namespace.Data + s).iri()
      }
    }
  }

  def isParserSide: Boolean =
    targetInstance.nonEmpty && targetInstance.head == PARSER_SIDE_VALIDATION

  def withTargets(other: ValidationSpecification): ValidationSpecification =
    copy(
      targetInstance = (this.targetInstance ++ other.targetInstance).distinct,
      targetClass = (this.targetClass ++ other.targetClass).distinct,
      targetObject = (this.targetObject ++ other.targetObject).distinct
    )

  /** Add a Target instance */
  def withTarget(targetId: String): ValidationSpecification =
    if (targetInstance contains targetId) this else copy(targetInstance = targetInstance :+ targetId)
}

object ValidationSpecification {
  val CORE_VALIDATION: String            = (Namespace.Shapes + "CoreShape").iri()
  val AML_VALIDATION: String             = (Namespace.Shapes + "AmlShape").iri()
  val PARSER_SIDE_VALIDATION: String     = (Namespace.Shapes + "ParserShape").iri()
  val PAYLOAD_VALIDATION: String         = (Namespace.Shapes + "PayloadShape").iri()
  val RENDER_SIDE_VALIDATION: String     = (Namespace.Shapes + "RenderShape").iri()
  val RESOLUTION_SIDE_VALIDATION: String = (Namespace.Shapes + "ResolutionShape").iri()
}
