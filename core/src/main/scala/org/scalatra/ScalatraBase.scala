package org.scalatra

import scala.collection.mutable.ListBuffer
import scala.util.DynamicVariable
import scala.collection.JavaConverters._

object ScalatraBase {
  val ParamsRequestKey: String = "org.scalatra.ScalatraBase.params"
  val MultiParamsRequestKey: String = "org.scalatra.ScalatraBase.multiParams"
}

trait ScalatraBase extends ResultConverters {
  import ScalatraBase._

  private[scalatra] val beforeActions = new ListBuffer[Action[_]]()
  private[scalatra] val actions       = new ListBuffer[Action[_]]()
  private[scalatra] val afterActions  = new ListBuffer[Action[_]]()

  private[scalatra] val requestHolder   = new DynamicVariable[ScalatraRequest](null)
  private[scalatra] val pathParamHolder = new DynamicVariable[Map[String, Seq[String]]](null)

  protected implicit def request: ScalatraRequest = {
    if(requestHolder.value == null){
      throw new ScalatraException("There needs to be a request in scope to call locale")
    }
    requestHolder.value
  }

  protected def params: Map[String, String] = {
    request.get(ParamsRequestKey).getOrElse {
      val params = multiParams.map { case (name, values) => name -> values.head }
      request.set(ParamsRequestKey, params)
      params
    }.asInstanceOf[Map[String, String]]
  }

  protected def multiParams: Map[String, Seq[String]] = {
    request.get(MultiParamsRequestKey).getOrElse {
      val params = request.underlying.getParameterNames.asScala.map { name =>
        name -> request.underlying.getParameterValues(name).toSeq
      }.toMap ++ pathParamHolder.value
      request.set(MultiParamsRequestKey, params)
      params
    }.asInstanceOf[Map[String, Seq[String]]]
  }

  protected def cookies: Cookies = {
    request.cookies
  }

  protected def sessions: Sessions = {
    request.sessions
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
