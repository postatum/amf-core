package amf.core.parser.errorhandler

import amf.plugins.features.validation.CoreValidations.SyamlWarning
import org.mulesoft.lexer.SourceLocation
import org.yaml.model.{DefaultJsonErrorHandler, SyamlException}


case class JsonErrorHandler(override val errorHandler: ParserErrorHandler) extends DefaultJsonErrorHandler {

  override protected def onIgnoredException(location: SourceLocation, e: SyamlException): Unit = errorHandler.warning(SyamlWarning, "", e.getMessage, location)
}
