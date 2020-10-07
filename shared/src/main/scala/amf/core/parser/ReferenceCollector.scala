package amf.core.parser

import amf.core.model.document.BaseUnit

import scala.collection.mutable

trait CollectionSideEffect[T] {
  def onCollect(alias: String, unit: T): Unit
}

trait ReferenceCollector[T] {
  def +=(alias: String, unit: T): Unit
  def nonEmpty: Boolean
  def references(): Seq[T]
  def get(alias: String): Option[T]
  def baseUnitReferences(): Seq[BaseUnit] = references().filter(_.isInstanceOf[BaseUnit]).map(_.asInstanceOf[BaseUnit])
}

case class CallbackReferenceCollector[T](private val sideEffect: CollectionSideEffect[T]) extends ReferenceCollector[T] {

  private val collector = DefaultReferenceCollector[T]()

  override def +=(alias:  String, unit:  T): Unit = {
    collector += (alias, unit)
    sideEffect.onCollect(alias, unit)
  }

  override def nonEmpty: Boolean = collector.nonEmpty
  override def get(alias: String): Option[T] = collector.get(alias)
  override def references(): scala.Seq[T] = collector.references()
}

case class DefaultReferenceCollector[T]() extends ReferenceCollector[T] {
  private val referenceMap: mutable.Map[String, T] = mutable.LinkedHashMap()

  override def +=(alias: String, unit: T): Unit = referenceMap += (alias -> unit)

  override def nonEmpty: Boolean = referenceMap.nonEmpty
  override def get(alias: String): Option[T] = referenceMap.get(alias)
  override def references(): Seq[T] = referenceMap.values.toSeq.distinct
}