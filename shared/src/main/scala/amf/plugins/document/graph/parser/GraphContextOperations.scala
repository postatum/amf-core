package amf.plugins.document.graph.parser

import scala.annotation.tailrec

trait GraphContextOperations {

  private val CompactUri = "(.*):(.*)".r

  def isExpandedIri(property: String): Boolean = property.startsWith("http://")

  def isCompactIri(property: String): Boolean = !isExpandedIri(property) && property.matches(CompactUri.regex)

  @tailrec
  final def expand(iri: String)(context: GraphContext): String = {
    iri match {
      case expandedIri if isExpandedIri(iri) => expandedIri
      case CompactUri(term, suffix) =>
        context.define(term) match {
          case Some(SimpleTermDefinition(namespace)) => s"$namespace$suffix"
          case _                                     => iri // legacy fallback, maybe report error
        }
      case alias =>
        val aliased = context.define(alias) match {
          case Some(SimpleTermDefinition(iri))                                         => iri
          case Some(expandedTerm: ExpandedTermDefinition) if expandedTerm.id.isDefined => expandedTerm.id.get
          case _                                                                       => iri // legacy fallback, maybe report error
        }
        aliased match {
          case CompactUri(_, _) => expand(aliased)(context) // we can have aliases for compact uris
          case _                => aliased
        }
    }
  }

  def aliasFor(iri: String)(context: GraphContext): Option[String] = {
    implicit val ordering: Ordering[TermDefinition] = ExpandedTermDefinitionsFirst
    val definitions                                 = context.definitions().toStream.sortBy { case (_, definition) => definition }

    definitions.collectFirst {
      case (alias, term: ExpandedTermDefinition) if term.id.exists(id => equal(iri, id)(context))               => alias
      case (alias, simpleTermDefinition: SimpleTermDefinition) if equal(iri, simpleTermDefinition.iri)(context) => alias
    }
  }

  def namespaceFor(iri: String)(context: GraphContext): Option[(String, String)] = {
    context.definitions().collectFirst {
      case (term, simpleTermDefinition: SimpleTermDefinition) if iri.startsWith(simpleTermDefinition.iri) =>
        (term, simpleTermDefinition.iri)
    }
  }

  def compact(iri: String)(context: GraphContext): String = {
    aliasFor(iri)(context)
      .orElse {
        namespaceFor(iri)(context).map {
          case (term, namespaceIri) => s"$term:${iri.stripPrefix(namespaceIri)}"
        }
      }
      .getOrElse {
        iri
      }
  }

  def equal(iriA: String, iriB: String)(context: GraphContext): Boolean = expand(iriA)(context) == expand(iriB)(context)

}

object ExpandedTermDefinitionsFirst extends Ordering[TermDefinition] {
  override def compare(x: TermDefinition, y: TermDefinition): Int = {
    (x, y) match {
      case (_: ExpandedTermDefinition, _: ExpandedTermDefinition) => 0
      case (_: SimpleTermDefinition, _: SimpleTermDefinition)     => 0
      case (_: ExpandedTermDefinition, _: SimpleTermDefinition)   => -1
      case (_: SimpleTermDefinition, _: ExpandedTermDefinition)   => 1
      case _                                                      => 0
    }
  }
}
