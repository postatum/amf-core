package amf.core.remote

import amf.core.model.document.BaseUnit

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

object GlobalCounter {
  var v = 0
}

class Cache {

  protected val cache: mutable.Map[String, Future[BaseUnit]]      = mutable.Map()
  protected val dependencyGraph: mutable.Map[String, Set[String]] = mutable.Map()

  protected def addFromToEdge(from: String, to: String): Unit = {
    val fromNodes = dependencyGraph.getOrElse(to, Set())
    dependencyGraph.update(to, fromNodes.+(from))
  }

  protected def findCycles(node: String, acc: Set[String] = Set()): Boolean = {
    if (acc.contains(node)) {
      true
    }
    else {
      val sources = dependencyGraph.getOrElse(node, Set())
      sources.exists { source =>
        findCycles(source, acc + node)
      }
    }
  }

  protected def beforeLast(elms: List[String]): Option[String] = {
    val lastTwo = elms.takeRight(2)
    if (lastTwo.size == 2) {
      lastTwo.headOption
    }
    else {
      None
    }
  }

  def getOrUpdate(url: String, context: Context)(supplier: () => Future[BaseUnit])(
      implicit executionContext: ExecutionContext): Future[BaseUnit] = synchronized {
    beforeLast(context.history) foreach { from =>
      addFromToEdge(from, url)
    }
    if (findCycles(url)) {
      if (cache(url).isCompleted) {
        cache(url)
      }
      else {
        cache.remove(url)
        supplier() map { res =>
          update(url, Future(res))
          res
        }
      }
    }
    else {
        cache.get(url) match {
          case Some(value) =>
            value
          case None =>
            val futureUnit = supplier()
            update(url, futureUnit)
            futureUnit
        }
    }
  }

  private def update(url: String, value: Future[BaseUnit]): Unit = synchronized {
    cache.update(url, value)
  }

  protected def size: Int = cache.size
}

object Cache {
  def apply(): Cache = {
    new Cache()
  }
}
