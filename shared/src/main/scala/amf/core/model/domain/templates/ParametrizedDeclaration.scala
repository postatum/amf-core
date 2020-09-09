package amf.core.model.domain.templates

import amf.core.metamodel.Field
import amf.core.metamodel.domain.templates.ParametrizedDeclarationModel._
import amf.core.model.domain.NamedDomainElement
import amf.core.parser.{Annotations, Fields}
import amf.core.utils.AmfStrings

abstract class ParametrizedDeclaration(fields: Fields, annotations: Annotations) extends NamedDomainElement {

  def target: AbstractDeclaration   = fields.field(Target)
  def variables: Seq[VariableValue] = fields.field(Variables)

  def withTarget(target: AbstractDeclaration): this.type      = set(Target, target)
  def withVariables(variables: Seq[VariableValue]): this.type = setArray(Variables, variables)

  override def componentId: String = "/" + name.option().getOrElse("default-parametrized").urlComponentEncoded

  override def nameField: Field = Name
}
