package org.scalatra

import scala.collection.mutable.ListBuffer
import scala.util.DynamicVariable
import scala.collection.JavaConverters._

trait ScalatraBase extends ResultConverters with ActionInterruptions {

  private[scalatra] val beforeActions = new ListBuffer[Action[_]]()
  private[scalatra] val actions       = new ListBuffer[Action[_]]()
  private[scalatra] val afterActions  = new ListBuffer[Action[_]]()

  private[scalatra] val requestHolder   = new DynamicVariable[ScalatraRequest](null)
  private[scalatra] val pathParamHolder = new DynamicVariable[Map[String, Seq[String]]](null)

  private val MultiParamsRequestKey = "org.scalatra.ScalatraBase.multiParams"

  protected implicit def request: ScalatraRequest = {
    if(requestHolder.value == null){
      throw new ScalatraException("There needs to be a request in scope to call locale")
    }
    requestHolder.value
  }

  protected def params: Map[String, String] = {
    multiParams.map { case (name, values) => name -> values.head }
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

  def cookies: Cookies = {
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
