package amf.core.remote
import java.net.{URI, URLDecoder}
import java.nio.charset.Charset

import amf.client.resource.{FileResourceLoader, HttpResourceLoader}
import amf.core.unsafe.PlatformBuilder
import amf.internal.resource.{ResourceLoader, ResourceLoaderAdapter}
import org.mulesoft.common.io.{FileSystem, Fs}

class JvmPlatform extends Platform {

  /** Underlying file system for platform. */
  override val fs: FileSystem = Fs

  /** Platform out of the box [ResourceLoader]s */
  override def loaders(): Seq[ResourceLoader] = Seq(
    ResourceLoaderAdapter(FileResourceLoader()),
    ResourceLoaderAdapter(HttpResourceLoader())
  )

  /** Return temporary directory. */
  override def tmpdir(): String = System.getProperty("java.io.tmpdir")

  /** Location where the helper functions for custom validations must be retrieved */
  override def customValidationLibraryHelperLocation: String = "classpath:validations/amf_validation.js"

  override def resolvePath(path: String): String = {
    val res = new URI(path).normalize.toString
    if (res.startsWith("file://") || res.startsWith("file:///")) {
      res
    } else if (res.startsWith("file:/")) {
      res.replace("file:/", "file:///")
    } else {
      res
    }
  }

  override def findCharInCharSequence(stream: CharSequence)(p: Char => Boolean): Option[Char] = {
    stream.chars().filter(c => p(c.toChar)).findFirst() match {
      case optInt if optInt.isPresent => Some(optInt.getAsInt.toChar)
      case _                          => None
    }
  }

  /** encodes a complete uri. Not encodes chars like / */
  override def encodeURI(url: String): String = // URLEncoder.encode(url, Charset.defaultCharset().toString)
    url
      .replaceAll("\\|", "%7C")
      .replaceAll(" ", "%20")
      .replaceAll("\\{", "%7B")
      .replaceAll("\\}", "%7D")
      .replaceAll("<", "%3C")
      .replaceAll(">", "%3E")
      .replaceAll("\\[", "%5B")
      .replaceAll("\\]", "%5D") // todo: add

  /** encodes a uri component, including chars like / and : */
  override def encodeURIComponent(url: String): String =
    url
      .replaceAll("\\|", "%7C")
      .replaceAll(":", "%3A")
      .replaceAll("/", "%2F")
      .replaceAll(" ", "%20")
      .replaceAll("\\{", "%7B")
      .replaceAll("\\}", "%7D")
      .replaceAll("<", "%3C")
      .replaceAll(">", "%3E")

  /** decode a complete uri. */
  override def decodeURI(url: String): String = URLDecoder.decode(url, Charset.defaultCharset().toString)

  /** decodes a uri component */
  override def decodeURIComponent(url: String): String =
    url
      .replaceAll("%7C", "\\|")
      .replaceAll("%3A", ":")
      .replaceAll("%2F", "/")
      .replaceAll("%20", " ")
      .replaceAll("%7B", "\\{")
      .replaceAll("%7D", "\\}")
      .replaceAll("%3C", "<")
      .replaceAll("%3E", ">")

  override def normalizeURL(url: String): String = new URI(encodeURI(url)).normalize.toString
}

object JvmPlatform {
  private var singleton: Option[JvmPlatform] = None

  def instance(): JvmPlatform = singleton match {
    case Some(p) => p
    case None =>
      singleton = Some(PlatformBuilder())
      singleton.get
  }
}
