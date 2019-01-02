package org.scalatra

import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.AtomicReference

import org.scalatra.util.UrlCodingUtils

import scala.collection.mutable.ListBuffer
import scala.util.DynamicVariable

object ScalatraBase {
  private[scalatra] val RequestAttributeParamsKey = "org.scalatra.params"
  private[scalatra] val RequestAttributeMultiParamsKey = "org.scalatra.multiParams"
}

trait Initializable {
  def init(config: Map[String, String]): Unit = {
  }
}

abstract class ScalatraBase extends ResultConverters with Initializable {
  import ScalatraBase._

  private[scalatra] val beforeActions = new ListBuffer[Action[_]]()
  private[scalatra] val actions       = new ListBuffer[Action[_]]()
  private[scalatra] val afterActions  = new ListBuffer[Action[_]]()

  private[scalatra] val requestHolder   = new DynamicVariable[ScalatraRequest](null)
  private[scalatra] val responseHolder  = new DynamicVariable[ScalatraResponse](null)
  private[scalatra] val pathParamHolder = new DynamicVariable[Map[String, Seq[String]]](null)

  protected implicit def request: ScalatraRequest = {
    if(requestHolder.value == null){
      throw new ScalatraException("There needs to be a request in scope to call locale")
    }
    requestHolder.value
  }

  protected implicit def response: ScalatraResponse = {
    if(responseHolder.value == null){
      throw new ScalatraException("There needs to be a response in scope to call locale")
    }
    responseHolder.value
  }

  protected def params: Map[String, String] = {
    request.get(RequestAttributeParamsKey).getOrElse {
      val params = multiParams.map { case (name, values) => name -> values.head }
      request.set(RequestAttributeParamsKey, params)
      params
    }.asInstanceOf[Map[String, String]]
  }

  protected def multiParams: Map[String, Seq[String]] = {
    request.get(RequestAttributeMultiParamsKey).getOrElse {
      // TODO How to determine a charset for url decoding?
      val queryParams = parseQueryParams(request.queryString)

      val bodyParams = if(request.headers.get("Content-Type").exists(_.toUpperCase.split(";")(0) == "APPLICATION/X-WWW-FORM-URLENCODED")){
        parseQueryParams(request.body)
      } else Map.empty

      val params = queryParams ++ bodyParams ++ pathParamHolder.value

      request.set(RequestAttributeMultiParamsKey, params)
      params
    }.asInstanceOf[Map[String, Seq[String]]]
  }

  private def parseQueryParams(queryString: String): Map[String, Seq[String]] = {
    if(queryString != null){
      queryString.split("&").flatMap { pair =>
        pair.split("=") match {
          case Array(key, value) => Some((
            UrlCodingUtils.urlDecode(key, StandardCharsets.UTF_8, plusIsSpace = true, skip = Set.empty[Int]),
            UrlCodingUtils.urlDecode(value, StandardCharsets.UTF_8, plusIsSpace = true, skip = Set.empty[Int])
          ))
          case _ => None
        }
      }.groupBy(_._1).map { case (key, values) =>
        key -> values.map(_._2).toSeq
      }
    } else Map.empty
  }

  protected def cookies: Cookies = {
    request.cookies
  }

  protected def before(f: => Unit): Unit = {
    val action = new Action(this, None, None, UnitResultConverter.convert(f))
    registerBeforeAction(action)
  }

  protected def before(path: String)(f: => Unit): Unit = {
    val action = new Action(this, Some(path), None, UnitResultConverter.convert(f))
    registerBeforeAction(action)
  }

  protected def after(f: => Unit): Unit = {
    val action = new Action(this, None, None, UnitResultConverter.convert(f))
    registerAfterAction(action)
  }

  protected def after(path: String)(f: => Unit): Unit = {
    val action = new Action(this, Some(path), None, UnitResultConverter.convert(f))
    registerAfterAction(action)
  }

  protected def get[T](path: String)(f: => T)(implicit converter: ResultConverter[T]) = {
    val action = new Action(this, Some(path), Some(Method.Get), converter.convert(f))
    registerAction(action)
  }

  protected def post[T](path: String)(f: => T)(implicit converter: ResultConverter[T]) = {
    val action = new Action(this, Some(path), Some(Method.Post), converter.convert(f))
    registerAction(action)
  }

  protected def put[T](path: String)(f: => T)(implicit converter: ResultConverter[T]) = {
    val action = new Action(this, Some(path), Some(Method.Put), converter.convert(f))
    registerAction(action)
  }

  protected def delete[T](path: String)(f: => T)(implicit converter: ResultConverter[T]) = {
    val action = new Action(this, Some(path), Some(Method.Delete), converter.convert(f))
    registerAction(action)
  }

  protected def head[T](path: String)(f: => T)(implicit converter: ResultConverter[T]) = {
    val action = new Action(this, Some(path), Some(Method.Head), converter.convert(f))
    registerAction(action)
  }

  protected def halt[T](status: java.lang.Integer = null, body: T = (), headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]): Unit = {
    val result = converter.convert(body)

    throw new HaltException(converter.convert(body).copy(
      status  = if(status == null) result.status else status,
      headers = result.headers ++ headers
    ))
  }

  protected def halt(result: ActionResult): Unit = {
    throw new HaltException(result)
  }

  protected def redirect(path: String): Unit = {
    halt(Found(path))
  }

  protected def pass(): Unit = {
    throw new PassException()
  }

  protected def registerAction(action: Action[_]): Unit = {
    actions += action
  }

  protected def registerBeforeAction(action: Action[_]): Unit = {
    beforeActions += action
  }

  protected def registerAfterAction(action: Action[_]): Unit = {
    afterActions += action
  }

}
