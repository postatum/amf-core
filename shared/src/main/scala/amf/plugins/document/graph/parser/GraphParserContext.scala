package amf.plugins.document.graph.parser

import amf.core.parser.errorhandler.AmfParserErrorHandler
import amf.core.parser.{EmptyFutureDeclarations, FutureDeclarations, ParsedReference, ParserContext}

class GraphParserContext(rootContextDocument: String = "",
                         refs: Seq[ParsedReference] = Seq.empty,
                         futureDeclarations: FutureDeclarations = EmptyFutureDeclarations(),
                         eh: AmfParserErrorHandler,
                         val graphContext: GraphContext = GraphContext())
    extends ParserContext(rootContextDocument, refs, futureDeclarations, eh) {}
