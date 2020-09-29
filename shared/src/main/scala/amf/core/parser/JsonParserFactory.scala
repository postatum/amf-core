package amf.core.parser

import amf.core.parser.errorhandler.{JsonErrorHandler, ParserErrorHandler}
import org.mulesoft.lexer.{Position => SyamlPosition}
import org.yaml.parser.JsonParser

object JsonParserFactory {

  def fromChars(s: CharSequence)(implicit errorhandler: ParserErrorHandler): JsonParser =
    JsonParser(s)(JsonErrorHandler(errorhandler))

  def fromCharsWithSource(s: CharSequence, sourceName: String, positionOffset: SyamlPosition = SyamlPosition.Zero)(
      implicit errorHandler: ParserErrorHandler): JsonParser =
    JsonParser.withSource(s, sourceName, positionOffset)(JsonErrorHandler(errorHandler))
}
