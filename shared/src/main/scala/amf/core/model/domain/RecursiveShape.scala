package amf.core.model.domain

import amf.core.errorhandling.ErrorHandler
import amf.core.metamodel.Obj
import amf.core.metamodel.domain.RecursiveShapeModel
import amf.core.metamodel.domain.RecursiveShapeModel._
import amf.core.model.StrField
import amf.core.parser.{Annotations, Fields}
import amf.core.traversal.ModelTraversalRegistry
import amf.core.utils.AmfStrings

class RecursiveShape(override val fields: Fields, override val annotations: Annotations) extends Shape {

  private var internalFixpointTarget: Option[Shape] = None

  def fixpoint: StrField            = fields.field(FixPoint)
  def fixpointTarget: Option[Shape] = internalFixpointTarget

  def withFixpointTarget(target: Shape): this.type = {
    internalFixpointTarget = Some(target)
    this
  }

  def withFixPoint(shapeId: String): this.type = set(FixPoint, shapeId)

  override def cloneShape(recursionErrorHandler: Option[ErrorHandler],
                          recursionBase: Option[String],
                          traversal: ModelTraversalRegistry = ModelTraversalRegistry(),
                          cloneExamples: Boolean = false): Shape = {
    val cloned = RecursiveShape()
    cloned.id = this.id
    copyFields(recursionErrorHandler, cloned, None, traversal)
    internalFixpointTarget.foreach(cloned.withFixpointTarget)
    cloned
  }

  override def linkCopy(): Linkable = throw new Exception("Recursive shape cannot be linked")

  override def meta: Obj = RecursiveShapeModel

  /** Value , path + field value that is used to compose the id when the object its adopted */
  override def componentId: String = name.option().map(name => s"/${name.urlComponentEncoded}").getOrElse("") + "/recursive"

  /** apply method for create a new instance with fields and annotations. Aux method for copy */
  override protected def classConstructor: (Fields, Annotations) => Linkable with DomainElement = RecursiveShape.apply

  override def copyElement(): this.type = this

  override def copyShape(): this.type = {
    val copy = super.copyShape().withId(id)
    fixpointTarget.foreach(copy.withFixpointTarget)
    copy
  }
}

object RecursiveShape {

  def apply(fields: Fields, annotations: Annotations): RecursiveShape = new RecursiveShape(fields, annotations)

  def apply(): RecursiveShape = apply(Fields(), Annotations())

  def apply(annotations: Annotations): RecursiveShape = apply(Fields(), annotations)

  def apply(l: Linkable): RecursiveShape =
    apply(Fields(), l.annotations)
      .adopted(l.id)
      .withSupportsRecursion(l.supportsRecursion.value())
      .withFixPoint(l.id)
      .withFixpointTarget(l.effectiveLinkTarget().asInstanceOf[Shape])

  def apply(shape: Shape): RecursiveShape =
    apply(Fields(), shape.annotations)
      .withName(shape.name.option().getOrElse("default-recursion"))
      .adopted(shape.id)
      .withSupportsRecursion(shape.supportsRecursion.value())
      .withFixPoint(shape.id)
      .withFixpointTarget(shape)
}
