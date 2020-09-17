package amf.core.traversal

import amf.core.annotations.{LexicalInformation, SourceAST, SourceNode}
import amf.core.metamodel.document.DocumentModel
import amf.core.model.document.{BaseUnit, DeclaresModel}
import amf.core.model.domain._
import amf.core.parser.{FieldEntry, Value}

class TransformationTraversal(val transformation: TransformationData) {

  def traverse(element: AmfObject, traversalPath: TraversalPath = ObjectIdTraversalPath()): AmfObject = {
    if (!traversalPath.hasVisited(element)) traverseElement(element, traversalPath)
    else element
  }

  protected def traverseElement(element: AmfObject, traversalPath: TraversalPath): AmfObject = {
    // not visited yet
    if (transformation.predicate(element)) { // matches predicate, we transform
      transformation.transformation(element, false) match {
        case Some(transformed: AmfObject) => transformed
        case other                        => other.orNull
      }
    } else {
      // not matches the predicate, we traverse
      // we first process declarations, then the encoding
      traversalPath.traversed(element)
      val effectiveFields: Iterable[FieldEntry] = getSortedFieldsOf(element)
      effectiveFields.foreach {
        case fieldEntry @ FieldEntry(_, value) if value.value.isInstanceOf[AmfObject] =>
          traverseObjectEntry(element, traversalPath, fieldEntry)
        case fieldEntry @ FieldEntry(_, value) if value.value.isInstanceOf[AmfArray] =>
          traverseArrayEntry(element, traversalPath, fieldEntry)

        case _ => // ignore
      }
      element
    }
  }

  protected def getSortedFieldsOf(element: AmfObject) = {
    element match {
      case doc: DeclaresModel => doc.fields.fields().toSeq.sorted(new DeclaresModelFieldOrdering)
      case bu: BaseUnit       => bu.fields.fields().toSeq.sorted(new BaseUnitFieldOrdering)
      case _                  => element.fields.fields()
    }
  }

  protected def traverseObjectEntry(element: AmfObject, traversalPath: TraversalPath, fieldEntry: FieldEntry) = {
    val FieldEntry(field, value) = fieldEntry
    Option(traverse(fieldEntry.obj, traversalPath)) match {
      case Some(transformedValue: AmfObject) =>
        element.fields.setWithoutId(field, transformedValue, lexicalAnnotationsOf(value))
      case Some(_) => // ignore
      case None    => element.fields.removeField(field)
    }
  }

  private def lexicalAnnotationsOf(value: Value) =
    value.annotations.copyFiltering(a =>
      a.isInstanceOf[LexicalInformation] || a.isInstanceOf[SourceAST] || a.isInstanceOf[SourceNode])

  protected def traverseArrayEntry(element: AmfObject, traversalPath: TraversalPath, fieldEntry: FieldEntry) = {
    val FieldEntry(field, value) = fieldEntry
    val newElements = fieldEntry.array.values
      .map {
        case elem: AmfObject =>
          val transformedValue = traverse(elem, traversalPath)
          Some(transformedValue)
        case other =>
          Some(other)
      }
      .filter(_.isDefined)
      .map(_.get)
    element.fields.setWithoutId(field, AmfArray(newElements, value.value.annotations), value.annotations)
  }
}

/**
  * Holder for transformation data in transform by condition
  * @param predicate selector
  * @param transformation transformation function
  */
sealed case class TransformationData(predicate: AmfObject => Boolean,
                                     transformation: (AmfObject, Boolean) => Option[AmfObject])


class DeclaresModelFieldOrdering extends Ordering[FieldEntry] {
  override def compare(x: FieldEntry, y: FieldEntry): Int = (x.field, y.field) match {
    case (DocumentModel.References, _) => -1
    case (_, DocumentModel.References) => 1
    case (DocumentModel.Declares, _)   => -1
    case (_, DocumentModel.Declares)   => 1
    case (_, _)                        => 0
  }
}

class BaseUnitFieldOrdering extends Ordering[FieldEntry] {
  override def compare(x: FieldEntry, y: FieldEntry): Int = (x.field, y.field) match {
    case (DocumentModel.References, _) => -1
    case (_, DocumentModel.References) => 1
    case (_, _)                        => 0
  }
}

class DomainElementSelectorAdapter(selector: DomainElement => Boolean) extends (AmfObject => Boolean) {
  override def apply(obj: AmfObject): Boolean = obj match {
    case e: DomainElement => selector(e)
    case _                => false
  }
}

class DomainElementTransformationAdapter(transformation: (DomainElement, Boolean) => Option[AmfObject])
    extends ((AmfObject, Boolean) => Option[AmfObject]) {
  override def apply(obj: AmfObject, isCycle: Boolean): Option[AmfObject] = obj match {
    case e: DomainElement => transformation(e, isCycle)
    case _                => Some(obj)
  }
}
