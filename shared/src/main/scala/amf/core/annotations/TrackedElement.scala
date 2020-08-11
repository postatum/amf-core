package amf.core.annotations

import amf.core.model.domain._

/**
  * TrackedElement is used to register the original location of an example (parameter, payload, etc).
  * This information has to be saved as it is lost when examples are propagated to their corresponding shapes.
  */
case class TrackedElement(parents: Set[String]) extends EternalSerializedAnnotation with UriAnnotation {

  /** Extension name. */
  override val name: String = "tracked-element"

  /** Value as string. */
  override val value: String     = parents.mkString(",")
  override val uris: Seq[String] = parents.toSeq

  override def shorten(fn: String => String): Annotation = TrackedElement(parents.map(fn))
}

object TrackedElement extends AnnotationGraphLoader {
  override def unparse(value: String, objects: Map[String, AmfElement]): Option[Annotation] =
    Some(TrackedElement(value.split(",").toSet))

  def apply(parent: String): TrackedElement = TrackedElement(Set(parent))
}
