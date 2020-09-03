package amf.core.emitter

import amf.core.errorhandling.ErrorHandler
import amf.core.model.document.BaseUnit
import amf.core.model.domain.DomainElement
import amf.plugins.features.validation.CoreValidations
import org.yaml.model.{YDocument, YNode}

trait DomainElementEmitter[T] {

  /**
    * @param emissionStructure: gives insight on the context of emission.
    * @param references: optional parameter which will improve emission of references defined in element.
    */
  def emit(element: DomainElement, emissionStructure: T, eh: ErrorHandler, references: Seq[BaseUnit] = Nil): YNode

  protected def nodeOrError(emitter: Option[PartEmitter], id: String, eh: ErrorHandler): YNode = {
    emitter
      .map { emitter =>
        YDocument(b => emitter.emit(b)).node
      }
      .getOrElse {
        eh.violation(CoreValidations.UnhandledDomainElement, id, "Unhandled domain element for given vendor")
        YNode.Empty
      }
  }
}
