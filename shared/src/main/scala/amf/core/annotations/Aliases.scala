package amf.core.annotations

import amf.core.model.document.BaseUnit
import amf.core.model.domain._
case class Aliases(aliases: Set[(Aliases.Alias, (Aliases.FullUrl, Aliases.RelativeUrl))])
    extends SerializableAnnotation
    with UriAnnotation {

  /** Extension name. */
  override val name: String = "aliases-array"

  /** Value as string. */
  override val value: String =
    aliases.map { case (alias, (fullUrl, relativeUrl)) => s"$alias->$fullUrl::$relativeUrl" }.mkString(",")
  override val uris: Seq[String] = aliases.map(_._2._1).toSeq

  override def shorten(fn: String => String): Annotation = {
    Aliases(aliases.map {
      case (alias, (fullUrl, relativeUrl)) =>
        alias -> (fn(fullUrl), relativeUrl)
    })
  }
}

object Aliases extends AnnotationGraphLoader {

  type FullUrl     = String
  type RelativeUrl = String
  type Alias       = String

  override def unparse(annotatedValue: String, objects: Map[String, AmfElement]): Option[Annotation] =
    Some(
      Aliases(
        annotatedValue
          .split(",")
          .map(_.split("->") match {
            case Array(alias, urls) =>
              urls.split("::") match {
                case Array(fullUrl, relativeUrl) =>
                  alias -> (fullUrl, relativeUrl)
              }
          })
          .toSet))
}

object AliasDeclaration {
  def apply(base: BaseUnit, aliasName: String, referenceId: String, relativeUrl: String): Unit = {
    val alias = (aliasName, (referenceId, relativeUrl))
    base.annotations.find(classOf[Aliases]) match {
      case Some(aliases) =>
        base.annotations.reject(_.isInstanceOf[Aliases])
        base.add(aliases.copy(aliases = aliases.aliases + alias))
      case None => base.add(Aliases(Set(alias)))
    }
  }
}
