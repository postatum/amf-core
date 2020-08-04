package amf.core.parser

import amf.core.model.document._
import amf.core.remote.File.FILE_PROTOCOL
import amf.core.remote.HttpParts.{HTTPS_PROTOCOL, HTTP_PROTOCOL}
import amf.core.utils.AmfStrings
import org.yaml.model.YNode.MutRef
import org.yaml.model.{YNode, YScalar}

import scala.collection.mutable

case class ReferenceResolutionResult(exception: Option[Throwable], unit: Option[BaseUnit])

case class RefContainer(linkType: ReferenceKind, node: YNode, fragment: Option[String]){

  def reduceToLocation(): Range = {
    node.asOption[YScalar] match {
      case Some(s)  =>
        reduceStringLength(s, fragment.map(l => l.length + 1).getOrElse(0), if(s.mark.plain) 0  else 1)
      case _ => Range(node.location.inputRange)
    }
  }

  private def reduceStringLength(s:YScalar, fragmentLenght:Int, markSize:Int = 0): Range = {
    val inputRange = if(node.location.inputRange.columnTo < fragmentLenght && node.location.inputRange.lineFrom< node.location.inputRange.lineTo) {
      val lines = s.text.split('\n')
      lines.find(_.contains('#')) match {
        case Some(line)  => node.location.inputRange.copy(lineTo = node.location.inputRange.lineFrom + lines.indexOf(line), columnTo = line.indexOf('#') -1 )
        case _ => node.location.inputRange
      }
    }else {
      getRefValue.location.inputRange.copy(columnTo = node.location.inputRange.columnTo - fragmentLenght)
    }
    Range((inputRange.lineFrom, inputRange.columnFrom + markSize), (inputRange.lineTo, inputRange.columnTo-markSize))
  }

  private def getRefValue = node match {
      case ref: MutRef => ref.origValue
      case _ => node
    }
}

case class ReferenceCollector() {
  private val refs = mutable.Map[String, Reference]()

  def +=(key: String, kind: ReferenceKind, node: YNode): Unit = {
    val (url, fragment) = ReferenceFragmentPartition(key)
    refs.get(url) match {
      case Some(reference: Reference) => refs.update(url, reference + (kind, node, fragment))
      case None                       => refs += url -> Reference(url, kind, node, fragment)
    }
  }

  def toReferences: Seq[Reference] = refs.values.toSeq
}

object EmptyReferenceCollector extends ReferenceCollector {}

/**
  * Splits references between their base url and local path
  * E.g. https://some.path.json#/local/path -> ("https://some.path.json", "local/path")
  */
object ReferenceFragmentPartition {
  // Is it always a URL? If we can have local references then it is not a URL
  def apply(url: String): (String, Option[String]) = {
    if (isExternalReference(url)) {
      url.split("#") match { // how can i know if the # its part of the uri or not? uri not valid???
        case Array(basePath) if basePath.endsWith("#") => (basePath.substring(0, basePath.length - 2), None)
        case Array(basePath)                           => (basePath, None)
        case Array(basePath, localPath)                => (basePath, Some(localPath))
        case other                                     =>
          //  -1 of the length diff and -1 for # char
          val str = url.substring(0, url.length - 1 - other.last.length)
          (str, Some(other.last))
      }
    }
    else (url, None)
  }

  /**
    * Checks if reference corresponds to a remote resource
    * @param referenceLocator like an URL but not uniform since we can have local references
    * @return
    */
  private def isExternalReference(referenceLocator: String) = {
    (referenceLocator.normalizeUrl.startsWith(FILE_PROTOCOL) || referenceLocator.startsWith(HTTPS_PROTOCOL) || referenceLocator
      .startsWith(HTTP_PROTOCOL)) && !referenceLocator
      .startsWith("#")
  }
}
