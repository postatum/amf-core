package amf.core.parser.errorhandler
import amf.core.errorhandling.{AmfResultErrorHandler, ErrorHandler}
import amf.plugins.features.validation.CoreValidations.SyamlError
import org.mulesoft.lexer.SourceLocation
import org.yaml.model._

trait ParserErrorHandler extends IllegalTypeHandler with ParseErrorHandler with ErrorHandler{

  private[amf] val parserRun: Int

  override def handle[T](error: YError, defaultValue: T): T = {
    violation(SyamlError, "", error.error, part(error))
    defaultValue
  }

  protected def part(error: YError): YPart = {
    error.node match {
      case d: YDocument => d
      case n: YNode     => n
      case s: YSuccess  => s.node
      case f: YFail     => part(f.error)
    }
  }

  final def handle(node: YPart, e: SyamlException): Unit = handle(node.location, e)

  override def handle(location: SourceLocation, e: SyamlException): Unit =
      violation(SyamlError, "", e.getMessage, location)
}


trait AmfParserErrorHandler extends AmfResultErrorHandler with ParserErrorHandler {}
