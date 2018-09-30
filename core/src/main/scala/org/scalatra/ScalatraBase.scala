package org.scalatra

import org.http4s._

import scala.collection.mutable.ListBuffer
import scala.util.DynamicVariable

trait ScalatraBase extends ResultConverters with ActionInterruptions {

  private[scalatra] val beforeActions = new ListBuffer[Action]()
  private[scalatra] val actions       = new ListBuffer[Action]()
  private[scalatra] val afterActions  = new ListBuffer[Action]()

  private[scalatra] val requestHolder   = new DynamicVariable[ScalatraRequest](null)
  private[scalatra] val pathParamHolder = new DynamicVariable[Map[String, Seq[String]]](null)

  protected implicit def request: ScalatraRequest = requestHolder.value

  protected def params: Map[String, String] = {
    multiParams.map { case (name, values) => name -> values.head }
  }

  protected def multiParams: Map[String, Seq[String]] = {
    requestHolder.value.underlying.multiParams ++ requestHolder.value.formParams ++ pathParamHolder.value
  }

  def cookies: scala.collection.mutable.Map[String, String] = requestHolder.value.cookies

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
    val action = new Action(this, Some(path), Some(Method.GET), converter.convert(f))
    registerAction(action)
  }

  protected def post[T](path: String)(f: => T)(implicit converter: ResultConverter[T]) = {
    val action = new Action(this, Some(path), Some(Method.POST), converter.convert(f))
    registerAction(action)
  }

  protected def put[T](path: String)(f: => T)(implicit converter: ResultConverter[T]) = {
    val action = new Action(this, Some(path), Some(Method.PUT), converter.convert(f))
    registerAction(action)
  }

  protected def delete[T](path: String)(f: => T)(implicit converter: ResultConverter[T]) = {
    val action = new Action(this, Some(path), Some(Method.DELETE), converter.convert(f))
    registerAction(action)
  }

  protected def head[T](path: String)(f: => T)(implicit converter: ResultConverter[T]) = {
    val action = new Action(this, Some(path), Some(Method.HEAD), converter.convert(f))
    registerAction(action)
  }

  protected def registerAction(action: Action): Unit = {
    actions += action
  }

  protected def registerBeforeAction(action: Action): Unit = {
    beforeActions += action
  }

  protected def registerAfterAction(action: Action): Unit = {
    afterActions += action
  }
}
