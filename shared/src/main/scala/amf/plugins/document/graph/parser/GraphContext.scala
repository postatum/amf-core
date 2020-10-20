package amf.plugins.document.graph.parser
import scala.collection.mutable

// This class represents a simplified version of an rdf graph context such as JSON-LD's @context
case class GraphContext() {

  var base: Option[StringIri] = None

  // Term -> Definition
  private val termDefinitions: mutable.Map[String, TermDefinition] = mutable.Map.empty

  def withTerm(term: String, iri: String): this.type = {
    termDefinitions.put(term, SimpleTermDefinition(iri))
    this
  }

  def withTerm(term: String, id: Option[String], `type`: Option[String]): this.type = {
    termDefinitions.put(term, ExpandedTermDefinition(id, `type`))
    this
  }

  def define(term: String): Option[TermDefinition] = termDefinitions.get(term)

  def withBase(iri: String): this.type = {
    this.base = Some(new StringIri(iri))
    this
  }

  def definitions(): Map[String, TermDefinition] = termDefinitions.toMap

}

class StringIri(val iri: String) {
  def parent: StringIri = {
    val cutoff = iri.lastIndexOf('/')
    new StringIri(iri.substring(0, cutoff))
  }
}

trait TermDefinition
case class SimpleTermDefinition(override val iri: String)                     extends StringIri(iri) with TermDefinition
case class ExpandedTermDefinition(id: Option[String], `type`: Option[String]) extends TermDefinition
