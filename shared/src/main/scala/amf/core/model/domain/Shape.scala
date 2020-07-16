package amf.core.model.domain

import amf.core.errorhandling.ErrorHandler
import amf.core.metamodel.Field
import amf.core.metamodel.domain.ShapeModel._
import amf.core.model.domain.extensions.{PropertyShape, ShapeExtension}
import amf.core.model.{BoolField, StrField}
import amf.core.parser.Annotations
import amf.core.traversal.ModelTraversalRegistry
import amf.plugins.domain.shapes.models.ShapeHelper

import scala.collection.mutable

/**
  * Shape.
  */
abstract class Shape extends DomainElement with Linkable with NamedDomainElement with ShapeHelper {

  override protected def nameField: Field = Name

  def displayName: StrField                              = fields.field(DisplayName)
  def description: StrField                              = fields.field(Description)
  def default: DataNode                                  = fields.field(Default)
  def defaultString: StrField                            = fields.field(DefaultValueString)
  def values: Seq[DataNode]                              = fields.field(Values)
  def inherits: Seq[Shape]                               = fields.field(Inherits)
  def or: Seq[Shape]                                     = fields.field(Or)
  def and: Seq[Shape]                                    = fields.field(And)
  def xone: Seq[Shape]                                   = fields.field(Xone)
  def not: Shape                                         = fields.field(Not)
  def customShapeProperties: Seq[ShapeExtension]         = fields.field(CustomShapeProperties)
  def customShapePropertyDefinitions: Seq[PropertyShape] = fields.field(CustomShapePropertyDefinitions)
  def readOnly: BoolField                                = fields.field(ReadOnly)
  def writeOnly: BoolField                               = fields.field(WriteOnly)
  def deprecated: BoolField                              = fields.field(Deprecated)
  def ifShape: Shape                                     = fields.field(If)
  def elseShape: Shape                                   = fields.field(Else)
  def thenShape: Shape                                   = fields.field(Then)

  def withDisplayName(name: String): this.type        = set(DisplayName, name)
  def withDescription(description: String): this.type = set(Description, description)
  def withDefault(default: DataNode): this.type       = set(Default, default)
  def withValues(values: Seq[DataNode]): this.type    = setArray(Values, values)
  def withInherits(inherits: Seq[Shape]): this.type   = setArray(Inherits, inherits)
  def withOr(subShapes: Seq[Shape]): this.type        = setArray(Or, subShapes)
  def withAnd(subShapes: Seq[Shape]): this.type       = setArray(And, subShapes)
  def withXone(subShapes: Seq[Shape]): this.type      = setArray(Xone, subShapes)
  def withNot(shape: Shape): this.type                = set(Not, shape)
  def withCustomShapeProperties(properties: Seq[ShapeExtension]): this.type =
    setArray(CustomShapeProperties, properties)
  def withCustomShapePropertyDefinitions(propertyDefinitions: Seq[PropertyShape]): this.type =
    setArray(CustomShapePropertyDefinitions, propertyDefinitions)
  def withCustomShapePropertyDefinition(name: String): PropertyShape = {
    val result = PropertyShape().withName(name, Annotations())
    add(CustomShapePropertyDefinitions, result)
    result
  }
  def withReadOnly(readOnly: Boolean): this.type     = set(ReadOnly, readOnly)
  def withWriteOnly(writeOnly: Boolean): this.type   = set(WriteOnly, writeOnly)
  def withDeprecated(deprecated: Boolean): this.type = set(Deprecated, deprecated)
  def withIf(ifShape: Shape): this.type              = set(If, ifShape)
  def withElse(elseShape: Shape): this.type          = set(Else, elseShape)
  def withThen(thenShape: Shape): this.type          = set(Then, thenShape)

  def withDefaultStr(value: String): Shape.this.type = set(DefaultValueString, value)

  def effectiveInherits: Seq[Shape] = {
    inherits.map { base =>
      if (base.linkTarget.isDefined) {
        base.effectiveLinkTarget() match {
          case linkedShape: Shape => linkedShape
          case _                  => base // TODO: what should we do here?
        }
      } else {
        base
      }
    } filter (_.id != id)
  }

  type FacetsMap = Map[String, PropertyShape]

  // @todo should be memoize this?
  def collectCustomShapePropertyDefinitions(onlyInherited: Boolean = false,
                                            traversed: mutable.Set[String] = mutable.Set()): Seq[FacetsMap] = {
    // Facet properties for the current shape
    val accInit: FacetsMap = Map.empty
    val initialSequence = if (onlyInherited) {
      Seq(accInit)
    } else {
      Seq(customShapePropertyDefinitions.foldLeft(accInit) { (acc: FacetsMap, propertyShape: PropertyShape) =>
        acc.updated(propertyShape.name.value(), propertyShape)
      })
    }

    // Check in the inheritance chain to add properties coming from super shapes and merging them with the facet
    // properties or properties for the current shape.
    // Notice that the properties map for this shape or from the inheritance can be sequences with more than one
    // element if unions are involved
    // inheritance will get the map of facet properties for each element in the union
    if (inherits.nonEmpty) {
      // for each base shape compute sequence(s) of facets map and merge it with the
      // initial facets maps computed for this shape. This multiplies the number of
      // final facets maps
      effectiveInherits.foldLeft(initialSequence) { (acc: Seq[FacetsMap], baseShape: Shape) =>
        if (!traversed.contains(baseShape.id)) {
          baseShape.collectCustomShapePropertyDefinitions(onlyInherited = false, traversed += baseShape.id).flatMap {
            facetsMap: FacetsMap =>
              acc.map { accFacetsMap =>
                accFacetsMap ++ facetsMap
              }
          }
        } else {
          acc
        }
      }
    } else {
      // no inheritance, return the initial sequence
      initialSequence
    }
  }

  def cloneShape(recursionErrorHandler: Option[ErrorHandler],
                 recursionBase: Option[String] = None,
                 traversed: ModelTraversalRegistry = ModelTraversalRegistry(),
                 cloneExample: Boolean = false): Shape

  // Copy fields into a cloned shape
  protected def copyFields(recursionErrorHandler: Option[ErrorHandler],
                           cloned: Shape,
                           recursionBase: Option[String],
                           traversal: ModelTraversalRegistry): Unit = {
    this.fields.foreach {
      case (f, v) =>
        val clonedValue = v.value match {
          case s: Shape if s.id != this.id && traversal.canTraverse(s.id) =>
            traversal.runNested(
              (t: ModelTraversalRegistry) => { s.cloneShape(recursionErrorHandler, recursionBase, t) })
          case s: Shape if s.id == this.id => s
          case a: AmfArray =>
            AmfArray(
              a.values.map {
                case e: Shape if e.id != this.id && traversal.canTraverse(e.id) =>
                  traversal.runNested((t: ModelTraversalRegistry) => {
                    e.cloneShape(recursionErrorHandler, recursionBase, t)
                  })
//                e.cloneShape(recursionErrorHandler, recursionBase, traversed.push(prevBaseId),Some(prevBaseId))
                case e: Shape if e.id == this.id => e
                case o                           => o
              },
              a.annotations
            )
          case o => o
        }

        cloned.fields.setWithoutId(f, clonedValue, v.annotations)
    }
  }

  def ramlSyntaxKey: String = "shape"

  def copyShape(): this.type = copyShape(annotations.copy())

  def copyShape(a: Annotations): this.type = {
    val copiedShape = copyElement(a).asInstanceOf[this.type]
    copiedShape
  }
}
