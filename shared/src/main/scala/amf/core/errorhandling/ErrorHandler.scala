package amf.core.errorhandling
import amf.core.annotations.{LexicalInformation, SourceLocation => AmfSourceLocation}
import amf.core.model.domain.AmfObject
import amf.core.parser.{Annotations, Range}
import amf.core.utils.AmfStrings
import amf.core.validation.SeverityLevels.{VIOLATION, WARNING}
import amf.core.validation.core.ValidationSpecification
import org.mulesoft.lexer.{InputRange, SourceLocation}
import org.yaml.model.YPart


trait ErrorHandler {

  def guiKey(message: String, location: Option[String], lexical: Option[LexicalInformation]): String = {
    message ++ location.getOrElse("") ++ lexical.map(_.value).getOrElse("")
  }

  def reportConstraint(id: String,
                       node: String,
                       property: Option[String],
                       message: String,
                       lexical: Option[LexicalInformation],
                       level: String,
                       location: Option[String]): Unit

  def reportConstraint(specification: ValidationSpecification, node: String, message: String, ast: YPart, level: String): Unit =
    reportConstraint(specification.id, node, None, message, lexical(ast.location), level, ast.sourceName.option)

  /** Report constraint failure of severity violation. */
  def violation(specification: ValidationSpecification,
                node: String,
                property: Option[String],
                message: String,
                lexical: Option[LexicalInformation],
                location: Option[String]): Unit = {
    reportConstraint(specification.id, node, property, message, lexical, VIOLATION, location)
  }

  def violation(specification: ValidationSpecification,
                node: String,
                message: String,
                annotations: Annotations): Unit = {
    violation(specification,
      node,
      None,
      message,
      annotations.find(classOf[LexicalInformation]),
      annotations.find(classOf[AmfSourceLocation]).map(_.location))
  }

  /** Report constraint failure of severity violation for the given amf object. */
  def violation(specification: ValidationSpecification,
                element: AmfObject,
                target: Option[String],
                message: String): Unit =
    violation(specification, element.id, target, message, element.position(), element.location())

  /** Report constraint failure of severity violation with location file. */
  def violation(specification: ValidationSpecification, node: String, message: String, location: String): Unit = {
    violation(specification, node, None, message, None, location.option)
  }

  /** Report constraint failure of severity violation. */
  def violation(spec: ValidationSpecification, node: String, prop: Option[String], msg: String, ast: YPart): Unit =
    violation(spec, node, prop, msg, ast.location)

  def violation(spec: ValidationSpecification, n: String, prop: Option[String], msg: String, l: SourceLocation): Unit =
    violation(spec, n, prop, msg, lexical(l), l.sourceName.option)

  /** Report constraint failure of severity violation. */
  def violation(specification: ValidationSpecification, node: String, message: String, ast: YPart): Unit =
    violation(specification, node, None, message, ast)

  def violation(specification: ValidationSpecification, node: String, message: String, loc: SourceLocation): Unit =
    violation(specification, node, None, message, loc)

  def violation(specification: ValidationSpecification, node: String, message: String): Unit =
    violation(specification, node, None, message, None, None)

  /** Report constraint failure of severity warning. */
  def warning(specification: ValidationSpecification,
              node: String,
              property: Option[String],
              message: String,
              lexical: Option[LexicalInformation],
              location: Option[String]): Unit =
    reportConstraint(specification.id, node, property, message, lexical, WARNING, location)

  /** Report constraint failure of severity violation for the given amf object. */
  def warning(spec: ValidationSpecification, element: AmfObject, target: Option[String], message: String): Unit =
    warning(spec, element.id, target, message, element.position(), element.location())

  /** Report constraint failure of severity warning. */
  def warning(specification: ValidationSpecification,
              node: String,
              property: Option[String],
              message: String,
              location: SourceLocation): Unit =
    warning(specification, node, property, message, lexical(location), location.sourceName.option)

  def warning(specification: ValidationSpecification,
              node: String,
              property: Option[String],
              message: String,
              part: YPart): Unit =
    warning(specification, node, property, message, part.location)

  /** Report constraint failure of severity warning. */
  def warning(specification: ValidationSpecification, node: String, message: String, ast: YPart): Unit =
    warning(specification, node, None, message, ast.location)

  def warning(specification: ValidationSpecification, node: String, message: String, location: SourceLocation): Unit =
    warning(specification, node, None, message, location)

  /** Report constraint failure of severity warning. */
  def warning(specification: ValidationSpecification, node: String, message: String, annotations: Annotations): Unit =
    warning(specification,
      node,
      None,
      message,
      annotations.find(classOf[LexicalInformation]),
      annotations.find(classOf[AmfSourceLocation]).map(_.location))


  private def lexical(loc: SourceLocation): Option[LexicalInformation] = {
    loc.inputRange match {
      case InputRange.Zero => None
      case range           => Some(LexicalInformation(Range(range)))
    }
  }


}