package amf.core

import java.net.URISyntaxException

import amf.client.parse.DefaultParserErrorHandler
import amf.client.plugins.AMFDocumentPlugin
import amf.client.remote.Content
import amf.core.TaggedReferences._
import amf.core.benchmark.ExecutionLog
import amf.core.client.ParsingOptions
import amf.core.exception.{CyclicReferenceException, UnsupportedMediaTypeException, UnsupportedVendorException}
import amf.core.model.document.{BaseUnit, ExternalFragment}
import amf.core.model.domain.ExternalDomainElement
import amf.core.parser.errorhandler.ParserErrorHandler
import amf.core.parser.{
  ParsedDocument,
  ParsedReference,
  ParserContext,
  RefContainer,
  ReferenceKind,
  ReferenceResolutionResult,
  UnspecifiedReference
}
import amf.core.registries.AMFPluginsRegistry
import amf.core.remote._
import amf.core.services.RuntimeCompiler
import amf.core.utils.AmfStrings
import amf.core.validation.core.ValidationSpecification
import amf.internal.environment.Environment
import amf.plugins.features.validation.CoreValidations._
import org.yaml.model.{YNode, YPart}

import scala.concurrent.Future.failed
import scala.concurrent.{ExecutionContext, Future}

object AMFCompilerRunCount {
  val NONE: Int = -1
  var count     = 0

  def nextRun(): Int = synchronized {
    count += 1
    count
  }
}

class CompilerContext(url: String,
                      val path: String,
                      val parserContext: ParserContext,
                      val fileContext: Context,
                      cache: Cache,
                      val environment: Environment)(implicit executionContext: ExecutionContext) {

  /**
    * The resolved path that result to be the normalized url
    */
  val location: String = fileContext.current

  def runInCache(fn: () => Future[BaseUnit]): Future[BaseUnit] = cache.getOrUpdate(location, fileContext)(fn)

  def logForFile(message: String): Unit = ExecutionLog.log(message + s" in $url")

  lazy val hasCycles: Boolean = fileContext.hasCycles

  lazy val platform: Platform = fileContext.platform

  def resolvePath(url: String): String = fileContext.resolve(fileContext.platform.normalizePath(url))

  def resolveContent()(implicit executionContext: ExecutionContext): Future[Content] =
    platform.resolve(location, environment)

  def forReference(url: String, withNormalizedUri: Boolean = true)(
      implicit executionContext: ExecutionContext): CompilerContext = {
    new CompilerContextBuilder(url, fileContext.platform, parserContext.eh)
      .withCache(cache)
      .withEnvironment(environment)
      .withBaseParserContext(parserContext)
      .withFileContext(fileContext)
      .withNormalizedUri(withNormalizedUri)
      .build()
  }

  def violation(id: ValidationSpecification, node: String, message: String, ast: YPart): Unit =
    parserContext.eh.violation(id, node, message, ast)

  def violation(id: ValidationSpecification, message: String, ast: YPart): Unit = violation(id, "", message, ast)

  val parserRun: Int = parserContext.parserRun

}

class CompilerContextBuilder(url: String,
                             platform: Platform,
                             eh: ParserErrorHandler = DefaultParserErrorHandler.withRun()) {

  private var fileContext: Context                = Context(platform)
  private var givenContent: Option[ParserContext] = None
  private var cache                               = Cache()
  private var environment                         = Environment()
  private var normalizeUri: Boolean               = true

  def withBaseParserContext(parserContext: ParserContext): this.type = {
    givenContent = Some(parserContext)
    this
  }

  def withFileContext(fc: Context): CompilerContextBuilder = {
    fileContext = fc
    this
  }

  def withCache(cache: Cache): CompilerContextBuilder = {
    this.cache = cache
    this
  }

  def withEnvironment(environment: Environment): CompilerContextBuilder = {
    this.environment = environment
    this
  }

  def withNormalizedUri(normalizeUri: Boolean): CompilerContextBuilder = {
    this.normalizeUri = normalizeUri
    this
  }

  /**
    * normalized url
    * */
  private val path: String = {
    try {
      url.normalizePath
    } catch {
      case e: URISyntaxException =>
        eh.violation(UriSyntaxError, url, e.getMessage)
        url
      case e: Exception => throw new PathResolutionError(e.getMessage)
    }
  }

  private def buildFileContext() = {
    val uriToUpdate = if (normalizeUri) path else url
    fileContext.update(uriToUpdate)
  }

  private def buildParserContext(fc: Context) = givenContent match {
    case Some(given) if given.rootContextDocument.equals(fc.current) => given
    case Some(given)                                                 => given.forLocation(fc.current)
    case None                                                        => ParserContext(fc.current, eh = eh)
  }

  def build()(implicit executionContext: ExecutionContext): CompilerContext = {
    val fc = buildFileContext()
    new CompilerContext(url, path, buildParserContext(fc), fc, cache, environment)
  }
}

class AMFCompiler(compilerContext: CompilerContext,
                  val mediaType: Option[String],
                  val vendor: Option[String],
                  val referenceKind: ReferenceKind = UnspecifiedReference,
                  val parsingOptions: ParsingOptions = ParsingOptions()) {

  def build()(implicit executionContext: ExecutionContext): Future[BaseUnit] = {
    compilerContext.logForFile(s"AMFCompiler#build: Building")
    if (compilerContext.hasCycles) failed(new CyclicReferenceException(compilerContext.fileContext.history))
    else
      compilerContext.runInCache(() => {
        compilerContext.logForFile(s"AMFCompiler#build: compiling")
        compile()
      })
  }

  private def compile()(implicit executionContext: ExecutionContext): Future[BaseUnit] =
    resolve().map(parseSyntax).flatMap(parseDomain)

  def autodetectSyntax(stream: CharSequence): Option[String] = {
    if (stream.length() > 2 && stream.charAt(0) == '#' && stream.charAt(1) == '%') {
      ExecutionLog.log(s"AMFCompiler#autodetectSyntax: auto detected application/yaml media type")
      Some("application/yaml")
    } else {
      compilerContext.platform.findCharInCharSequence(stream) { c =>
        c != '\n' && c != '\t' && c != '\r' && c != ' '
      } match {
        case Some(c) if c == '{' || c == '[' =>
          ExecutionLog.log(s"AMFCompiler#autodetectSyntax: auto detected application/json media type")
          Some("application/json")
        case _ => None
      }
    }
  }

  private def parseSyntax(input: Content): Either[Content, Root] = {
    compilerContext.logForFile("AMFCompiler#parseSyntax: parsing syntax")
    val content = AMFPluginsRegistry.featurePlugins().foldLeft(input) {
      case (c, p) =>
        p.onBeginDocumentParsing(compilerContext.path, c, referenceKind)
    }

    val parsed: Option[(String, ParsedDocument)] = mediaType
      .flatMap(mime => parseSyntaxForMediaType(content, mime))
      .orElse {
        mediaType match {
          case None =>
            content.mime
              .flatMap(mime => parseSyntaxForMediaType(content, mime))
              .orElse {
                FileMediaType
                  .extension(content.url)
                  .flatMap(FileMediaType.mimeFromExtension)
                  .flatMap(inferred => parseSyntaxForMediaType(content, inferred))
              }
              .orElse {
                autodetectSyntax(content.stream).flatMap(inferred => parseSyntaxForMediaType(content, inferred))
              }
          case _ => None
        }
      }

    parsed match {
      case Some((effective, document)) =>
        val doc = AMFPluginsRegistry.featurePlugins().foldLeft(document) {
          case (d, p) =>
            p.onSyntaxParsed(compilerContext.path, d)
        }
        Right(Root(doc, content.url, effective, Seq(), referenceKind, content.stream.toString))
      case None =>
        Left(content)
    }
  }

  private def parseSyntaxForMediaType(content: Content, mime: String) = {
    AMFPluginsRegistry
      .syntaxPluginForMediaType(mime)
      .flatMap(_.parse(mime, content.stream, compilerContext.parserContext, parsingOptions))
      .map((mime, _))
  }

  def parseExternalFragment(content: Content)(implicit executionContext: ExecutionContext): Future[BaseUnit] = Future {
    val result = ExternalDomainElement().withId(content.url + "#/").withRaw(content.stream.toString)
    content.mime.foreach(mime => result.withMediaType(mime))
    ExternalFragment()
      .withLocation(content.url)
      .withId(content.url)
      .withEncodes(result)
      .withLocation(content.url)
      .withRunNumber(compilerContext.parserRun)
  }

  private def parseDomain(parsed: Either[Content, Root])(
      implicit executionContext: ExecutionContext): Future[BaseUnit] = {
    parsed match {
      case Left(content) =>
        mediaType match {
          // if is Left (empty or other error) and is root (context.history.length == 1), then return an error
          case Some(mime) if compilerContext.fileContext.history.length == 1 =>
            throw new UnsupportedMediaTypeException(mime)
          case _ => parseExternalFragment(content)
        }
      case Right(document) => parseDomain(document)
    }
  }

  private def parseDomain(document: Root)(implicit executionContext: ExecutionContext): Future[BaseUnit] = {
    compilerContext.logForFile("AMFCompiler#parseDomain: parsing domain")

    val domainPluginOption = getDomainPluginFor(document)

    val futureDocument: Future[BaseUnit] = domainPluginOption match {
      case Some(domainPlugin) =>
        compilerContext.logForFile(s"AMFCompiler#parseSyntax: parsing domain plugin ${domainPlugin.ID}")
        parseReferences(document, domainPlugin) map { documentWithReferences =>
          val newCtx = compilerContext.parserContext.copyWithSonsReferences()
          domainPlugin.parse(documentWithReferences, newCtx, compilerContext.platform, parsingOptions) match {
            case Some(baseUnit) =>
              if (document.location == compilerContext.fileContext.root)
                baseUnit.withRoot(true)
              baseUnit
                .withRaw(document.raw)
                .tagReferences(documentWithReferences)

            case None => buildExternalFragment(document)
          }
        }
      case None if vendor.isDefined => throw new UnsupportedVendorException(vendor.get)
      case None =>
        Future {
          compilerContext.logForFile("AMFCompiler#parseSyntax: parsing domain NO PLUGIN")
          buildExternalFragment(document)
        }
    }

    futureDocument map { baseUnit: BaseUnit =>
      // we setup the run for the parsed unit
      baseUnit.withRunNumber(compilerContext.parserRun)
      compilerContext.logForFile("AMFCompiler#parseDomain: model ready")
      val bu = AMFPluginsRegistry.featurePlugins().foldLeft(baseUnit) {
        case (unit, plugin) =>
          plugin.onModelParsed(compilerContext.path, unit)
      }
      baseUnit
    }
  }

  private def buildExternalFragment(document: Root) = {
    ExternalFragment()
      .withId(document.location)
      .withLocation(document.location)
      .withEncodes(
        ExternalDomainElement()
          .withRaw(document.raw)
          .withMediaType(document.mediatype))
  }

  private def getDomainPluginFor(document: Root) = {
    vendor.fold(AMFPluginsRegistry.documentPluginForMediaType(document.mediatype).find(_.canParse(document)))({
      AMFPluginsRegistry.documentPluginForVendor(_).find(_.canParse(document))
    })
  }

  private def parseReferences(root: Root, domainPlugin: AMFDocumentPlugin)(
      implicit executionContext: ExecutionContext): Future[Root] = {
    val handler = domainPlugin.referenceHandler(compilerContext.parserContext.eh)
    val refs    = handler.collect(root.parsed, compilerContext.parserContext)
    compilerContext.logForFile(s"AMFCompiler#parseReferences: ${refs.toReferences.size} references found")
    val parsed: Seq[Future[Option[ParsedReference]]] = refs.toReferences
      .filter(_.isRemote)
      .map { link =>
        val nodes = link.refs.map(_.node)
        link.resolve(compilerContext, nodes, domainPlugin.allowRecursiveReferences) flatMap {
          case ReferenceResolutionResult(_, Some(unit)) =>
            verifyMatchingVendor(unit.sourceVendor, domainPlugin.vendors.map(Vendor.apply), nodes)
            verifyValidFragment(unit.sourceVendor, link.refs)
            val reference = ParsedReference(unit, link)
            handler.update(reference, compilerContext).map(Some(_))
          case ReferenceResolutionResult(Some(e), _) =>
            e match {
              case e: CyclicReferenceException if !domainPlugin.allowRecursiveReferences =>
                compilerContext.violation(CycleReferenceError, link.url, e.getMessage, link.refs.head.node)
                Future(None)
              case _ =>
                if (!link.isInferred) {
                  nodes.foreach(compilerContext.violation(UnresolvedReference, link.url, e.getMessage, _))
                }
                Future(None)
            }
          case _ => Future(None)
        }
      }

    Future.sequence(parsed).map(rs => root.copy(references = rs.flatten))
  }

  private def resolve()(implicit executionContext: ExecutionContext): Future[Content] = compilerContext.resolveContent()

  private def verifyMatchingVendor(refVendor: Option[Vendor], rootVendors: Seq[Vendor], nodes: Seq[YNode]): Unit =
    refVendor match {
      case Some(v) if !rootVendors.contains(v) && !isJsonRef(rootVendors) =>
        nodes.foreach(compilerContext.violation(InvalidCrossSpec, "Cannot reference fragments of another spec", _))
      case _ => // Nothing to do
    }

  private val JSON_REFS = "JSON + Refs"

  private def isJsonRef(vendors: Seq[Vendor]) = vendors.forall(_.name.equals(JSON_REFS))

  def verifyValidFragment(refVendor: Option[Vendor], refs: Seq[RefContainer]): Unit = refVendor match {
    case Some(v) if v.isRaml =>
      refs.foreach(
        r =>
          if (r.fragment.isDefined)
            compilerContext.violation(InvalidFragmentRef, "Cannot use reference with # in a RAML fragment", r.node))
    case _ => // Nothing to do
  }

  def root()(implicit executionContext: ExecutionContext): Future[Root] = resolve().map(parseSyntax).flatMap {
    case Right(document: Root) =>
      AMFPluginsRegistry.documentPluginForMediaType(document.mediatype).find(_.canParse(document)) match {
        case Some(domainPlugin) =>
          parseReferences(document, domainPlugin)
        case None =>
          Future {
            document
          }
      }
    case Left(content) =>
      throw new Exception(s"Cannot parse document with mime type ${content.mime.getOrElse("none")}")
  }

}

object AMFCompiler {
  def init()(implicit executionContext: ExecutionContext) {
    // We register ourselves as the Runtime compiler
    RuntimeCompiler.register(new AMFCompilerAdapter())
  }
}

class AMFCompilerAdapter(implicit executionContext: ExecutionContext) extends RuntimeCompiler {
  override def build(compilerContext: CompilerContext,
                     mediaType: Option[String],
                     vendor: Option[String],
                     referenceKind: ReferenceKind,
                     parsingOptions: ParsingOptions): Future[BaseUnit] = {
    new AMFCompiler(compilerContext, mediaType, vendor, referenceKind, parsingOptions).build()
  }
}

case class Root(parsed: ParsedDocument,
                location: String,
                mediatype: String,
                references: Seq[ParsedReference],
                referenceKind: ReferenceKind,
                raw: String) {}

object Root {
  def apply(parsed: ParsedDocument,
            location: String,
            mediatype: String,
            references: Seq[ParsedReference],
            referenceKind: ReferenceKind,
            raw: String): Root =
    new Root(parsed, location.normalizeUrl, mediatype, references, referenceKind, raw)
}
