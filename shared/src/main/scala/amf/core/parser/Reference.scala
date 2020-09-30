package amf.core.parser
import amf.client.plugins.{AMFDocumentPlugin, AMFDomainPlugin}
import amf.core.benchmark.ExecutionLog
import amf.core.exception.CyclicReferenceException
import amf.core.model.document.{BaseUnit, Module, RecursiveUnit}
import amf.core.remote.Vendor
import amf.core.services.RuntimeCompiler
import amf.core.unsafe.PlatformSecrets
import amf.core.vocabulary.Namespace
import amf.core.{CompilerContext, parser}
import amf.internal.environment.Environment
import amf.plugins.features.validation.CoreValidations.{ExpectedModule, InvalidInclude}
import org.yaml.model.{YNode, YScalar}

import scala.concurrent.{ExecutionContext, Future}

case class Reference(url: String, refs: Seq[RefContainer]) extends PlatformSecrets {

  def isRemote: Boolean = !url.startsWith("#")

  def +(kind: ReferenceKind, ast: YNode, fragment: Option[String]): Reference = {
    copy(refs = refs :+ RefContainer(kind, ast, fragment))
  }

  def resolve(compilerContext: CompilerContext, nodes: Seq[YNode], allowRecursiveRefs: Boolean, domainPlugin: AMFDocumentPlugin)(
      implicit executionContext: ExecutionContext): Future[ReferenceResolutionResult] = {

    // If there is any ReferenceResolver attached to the environment, then first try to get the cached reference if it exists. If not, load and parse as usual.
    try {
      compilerContext.environment.resolver match {
        case Some(resolver) =>
          resolver.fetch(compilerContext.resolvePath(url)) flatMap { cachedReference =>
            Future(ReferenceResolutionResult(None, Some(cachedReference.content)))
          } recoverWith {
            case _ => resolveReference(compilerContext, nodes, allowRecursiveRefs, domainPlugin)
          }
        case None => resolveReference(compilerContext, nodes, allowRecursiveRefs, domainPlugin)
      }
    } catch {
      case _: Throwable => resolveReference(compilerContext, nodes, allowRecursiveRefs, domainPlugin)
    }
  }

  private def resolveReference(compilerContext: CompilerContext, nodes: Seq[YNode], allowRecursiveRefs: Boolean, domainPlugin: AMFDocumentPlugin)(
      implicit executionContext: ExecutionContext): Future[ReferenceResolutionResult] = {
    val kinds = refs.map(_.linkType).distinct
    val kind  = if (kinds.size > 1) UnspecifiedReference else kinds.head
    try {
      val context = compilerContext.forReference(url)
      val res: Future[Future[ReferenceResolutionResult]] = RuntimeCompiler.forContext(context, None, None, kind) map {
        eventualUnit =>
          domainPlugin.verifyReferenceKind(eventualUnit, kind, kinds, nodes, context.parserContext)
          Future(parser.ReferenceResolutionResult(None, Some(eventualUnit)))
      } recover {
        case e: CyclicReferenceException if allowRecursiveRefs =>
          val fullUrl = e.history.last
          resolveRecursiveUnit(fullUrl, compilerContext).map(u => ReferenceResolutionResult(None, Some(u)))
        case e: Throwable =>
          Future(ReferenceResolutionResult(Some(e), None))
      }
      res flatMap identity
    } catch {
      case e: Throwable => Future(ReferenceResolutionResult(Some(e), None))
    }
  }

  protected def resolveRecursiveUnit(fulllUrl: String, compilerContext: CompilerContext)(
      implicit executionContext: ExecutionContext): Future[RecursiveUnit] = {
    ExecutionLog.log(s"AMFCompiler#parserReferences: Recursive reference $fulllUrl")
    platform.resolve(fulllUrl, compilerContext.environment) map { content =>
      val recUnit = RecursiveUnit().adopted(fulllUrl).withLocation(fulllUrl)
      recUnit.withRaw(content.stream.toString)
      recUnit
    }
  }

  def isInferred: Boolean = refs.exists(_.linkType == InferredLinkReference)
}
object Reference {
  def apply(url: String, kind: ReferenceKind, node: YNode, fragment: Option[String]): Reference =
    new Reference(url, Seq(RefContainer(kind, node, fragment)))
}
