package org.scalatra

import org.scalatra.util.StringUtils
import org.slf4j.LoggerFactory

object CorsSupport {

  // request headers
  val OriginHeader                       : String = "Origin"
  val AccessControlRequestMethodHeader   : String = "Access-Control-Request-Method"
  val AccessControlRequestHeadersHeader  : String = "Access-Control-Request-Headers"
  val AccessControlAllowOriginHeader     : String = "Access-Control-Allow-Origin"
  val AccessControlAllowMethodsHeader    : String = "Access-Control-Allow-Methods"
  val AccessControlAllowHeadersHeader    : String = "Access-Control-Allow-Headers"
  val AccessControlMaxAgeHeader          : String = "Access-Control-Max-Age"
  val AccessControlAllowCredentialsHeader: String = "Access-Control-Allow-Credentials"

  private val SimpleHeaders     : Seq[String] = List(OriginHeader.toUpperCase, "ACCEPT", "ACCEPT-LANGUAGE", "CONTENT-LANGUAGE")
  private val SimpleContentTypes: Seq[String] = List("APPLICATION/X-WWW-FORM-URLENCODED", "MULTIPART/FORM-DATA", "TEXT/PLAIN")

  private def isSimpleHeader(header: String, request: ScalatraRequest): Boolean = {
    StringUtils.blankOption(header).exists { header =>
      val upperCaseHeader = header.toUpperCase
      SimpleHeaders.contains(upperCaseHeader) || (upperCaseHeader == "CONTENT-TYPE" &&
        SimpleContentTypes.exists((request.contentType.getOrElse("")).toUpperCase.startsWith))
    }
  }

  val CorsHeaders: Seq[String] = List(
    OriginHeader,
    AccessControlAllowCredentialsHeader,
    AccessControlAllowHeadersHeader,
    AccessControlAllowMethodsHeader,
    AccessControlAllowOriginHeader,
    AccessControlMaxAgeHeader,
    AccessControlRequestHeadersHeader,
    AccessControlRequestMethodHeader
  )

  case class CORSConfig(
    allowedOrigins: Seq[String],
    allowedMethods: Seq[String],
    allowedHeaders: Seq[String],
    allowCredentials: Boolean,
    preflightMaxAge: Int = 0,
    enabled: Boolean)

  // configuration keys
  val AllowedOriginsKey  : String = "org.scalatra.cors.allowedOrigins"
  val AllowedMethodsKey  : String = "org.scalatra.cors.allowedMethods"
  val AllowedHeadersKey  : String = "org.scalatra.cors.allowedHeaders"
  val AllowCredentialsKey: String = "org.scalatra.cors.allowCredentials"
  val PreflightMaxAgeKey : String = "org.scalatra.cors.preflightMaxAge"
  val EnableKey          : String = "org.scalatra.cors.enable"
  val CorsConfigKey      : String = "org.scalatra.cors.corsConfig"

  // default configurations
  private val AnyOrigin: String = "*"
  private val DefaultMethods: String = "GET,POST,PUT,DELETE,HEAD,OPTIONS,PATCH"

  private val DefaultHeaders: String = Seq(
    "Cookie",
    "Host",
    "X-Forwarded-For",
    "Accept-Charset",
    "If-Modified-Since",
    "Accept-Language",
    "X-Forwarded-Port",
    "Connection",
    "X-Forwarded-Proto",
    "User-Agent",
    "Referer",
    "Accept-Encoding",
    "X-Requested-With",
    "Authorization",
    "Accept",
    "Content-Type").mkString(",")

  // Blank to None
  private def getRequestHeader(request: ScalatraRequest, name: String): Option[String] =
    StringUtils.blankOption(request.headers.get(name).orNull)

}

trait CorsSupport extends Initializable { this: ScalatraBase =>
  import CorsSupport._

  private[this] lazy val logger = LoggerFactory.getLogger(getClass)
  private var corsConfig: CORSConfig = null

  before {
    if(corsConfig.enabled && getRequestHeader(request, OriginHeader).isDefined){
      request.requestMethod match {
        case Options if isPreflightRequest => {
          handlePreflightRequest()
          halt()
        }
        case Get | Post | Head if isSimpleRequest => {
          augmentSimpleRequest()
        }
        case _ => {
          augmentSimpleRequest()
        }
      }
    }
  }

  override def init(config: Map[String, String]): Unit = {
    super.init(config)

    corsConfig = CORSConfig(
      allowedOrigins   = config.get(AllowedOriginsKey).getOrElse(AnyOrigin).split(",").map(_.trim),
      allowedMethods   = config.get(AllowedMethodsKey).getOrElse(DefaultMethods).split(",").map(_.trim),
      allowedHeaders   = config.get(AllowedHeadersKey).getOrElse(DefaultHeaders).split(",").map(_.trim),
      allowCredentials = config.get(AllowCredentialsKey).map(_.toBoolean).getOrElse(true),
      preflightMaxAge  = config.get(PreflightMaxAgeKey).map(_.toInt).getOrElse(1800),
      enabled          = config.get(EnableKey).map(_.toBoolean).getOrElse(true)
    )

    if (corsConfig.enabled) {
      logger.debug("Enabled CORS Support with:\nallowedOrigins:\n\t%s\nallowedMethods:\n\t%s\nallowedHeaders:\n\t%s".format(
        corsConfig.allowedOrigins mkString ", ",
        corsConfig.allowedMethods mkString ", ",
        corsConfig.allowedHeaders mkString ", "))
    } else {
      logger.debug("Cors support is disabled")
    }
  }

  protected def handlePreflightRequest(): Unit = {
    logger.debug("handling preflight request")

    augmentSimpleRequest()

    if (corsConfig.preflightMaxAge > 0){
      response.headers.set(AccessControlMaxAgeHeader, corsConfig.preflightMaxAge.toString)
    }
    response.headers.set(AccessControlAllowMethodsHeader, corsConfig.allowedMethods mkString ",")

    val accessControlRequestHeader = request.headers.get(AccessControlRequestHeadersHeader).map(_.split(",").toSeq).getOrElse(Nil)
    val allowedHeaders = corsConfig.allowedHeaders ++ accessControlRequestHeader
    response.headers.set(AccessControlAllowHeadersHeader, allowedHeaders.mkString(","))
  }

  protected def augmentSimpleRequest(): Unit = {
    val anyOriginAllowed: Boolean = corsConfig.allowedOrigins.contains(AnyOrigin)

    val allowedOrigin = if (anyOriginAllowed && !corsConfig.allowCredentials){
      Some(AnyOrigin)
    } else {
      request.headers.get(OriginHeader).filter(corsConfig.allowedOrigins.contains)
    }
    allowedOrigin.foreach { value =>
      response.headers.set(AccessControlAllowOriginHeader, value)
    }

    if (corsConfig.allowCredentials){
      response.headers.set(AccessControlAllowCredentialsHeader, "true")
    }

    request.headers.get(AccessControlRequestHeadersHeader).foreach { value =>
      response.headers.set(AccessControlAllowHeadersHeader, value)
    }
  }

  private[this] def isEnabled: Boolean =
    !("Upgrade".equalsIgnoreCase(request.headers.getOrElse("Connection", "")) &&
      "WebSocket".equalsIgnoreCase(request.headers.getOrElse("Upgrade", ""))) &&
      !request.underlying.getRequestURI.contains("eb_ping") // don't do anything for the ping endpoint

//  // TODO Implement this method correctly
//  private[this] def isValidRoute: Boolean = true //??? //routes.matchingMethods(requestPath).nonEmpty

  private[this] def isPreflightRequest: Boolean = {
    // Check Origin header
    val originMatches = corsConfig.allowedOrigins.contains(AnyOrigin) ||
      (corsConfig.allowedOrigins.contains(request.headers.getOrElse(OriginHeader, "")))

    // Check Access-Control-Request-Method header
    val allowsMethod = getRequestHeader(request, AccessControlRequestMethodHeader).exists { method =>
      corsConfig.allowedMethods.contains(method.toUpperCase)
    }

    // Check Access-Control-Request-Headers header
    val headersAreAllowed = {
      val allowedHeaders = corsConfig.allowedHeaders.map(_.trim.toUpperCase)
      val requestedHeaders = getRequestHeader(request, AccessControlRequestHeadersHeader).map(_.toUpperCase)
      requestedHeaders.forall { header => isSimpleHeader(header, request) || allowedHeaders.contains(header) }
    }

    isEnabled && originMatches && allowsMethod && headersAreAllowed
  }

  private[this] def isSimpleRequest: Boolean = {
    // Check Origin header
    val allOriginsMatch = getRequestHeader(request, OriginHeader).map { origin =>
      origin.split(" ") match {
        case origins if origins.isEmpty => false
        case origins => origins.forall(corsConfig.allowedOrigins.contains)
      }
    }.getOrElse(false)

    isEnabled && allOriginsMatch && request.headers.keys.forall { header => isSimpleHeader(header, request) }
  }

}