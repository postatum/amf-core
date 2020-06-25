package amf.core.parser

import amf.core.model.domain.Linkable

import scala.collection.mutable

case class DeclarationPromise(private val success: (Linkable) => Any,
                              private val failure: () => Any,
                              var resolved: Boolean = false) {

  def resolve(element: Linkable): Unit = {
    resolved = true
    success(element)
  }

  def fail(): Unit = {
    resolved = true
    failure()
  }
}

trait FutureDeclarations {

  val promises: mutable.Map[String, Seq[DeclarationPromise]] = mutable.Map()

  def futureRef(id: String, name: String, promise: DeclarationPromise): Unit = this.synchronized {
    val otherPromises = promises.getOrElse(name, Seq[DeclarationPromise]())
    promises.update(name, otherPromises ++ Seq(promise))
  }

  def resolveRef(name: String, value: Linkable): Unit = this.synchronized {
    promises.getOrElse(name, Seq[DeclarationPromise]()).foreach(_.resolve(value))
    promises.update(name, Seq[DeclarationPromise]())
  }

  /** Resolve all UnresolvedShape references or fail. */
  def resolve(): Unit = {
    // we fail unresolved references
    promises.values.flatten.filter(!_.resolved).foreach(_.fail())
  }

  def addDeclarationsFrom(context: ParserContext): FutureDeclarations = {
    context.futureDeclarations.promises.filter(t => t._2.nonEmpty).foreach {
      case (key: String, promisesList: Seq[DeclarationPromise]) =>
        val currentPromiseList = promises.getOrElse(key, Seq[DeclarationPromise]())
        promises.put(key, currentPromiseList ++ promisesList)
    }
    this
  }

}
