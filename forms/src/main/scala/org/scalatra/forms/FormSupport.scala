package org.scalatra.forms

import org.scalatra.i18n._
import org.scalatra.util.JsonUtil
import org.scalatra.{ActionResult, ByteArrayBody, ResultConverter, ScalatraBase}

import scala.collection.mutable.{Map => MutableMap}

trait FormSupport { self: ScalatraBase with I18nSupport =>

  protected def validate[T, R1, R2](form: ValueType[T])(hasErrors: Seq[(String, String)] => R1, success: T => R2)
    (implicit errorsConverter: ResultConverter[R1], successConverter: ResultConverter[R2]): ActionResult = {
    val params = multiParams
    request.set(RequestAttributeParamsKey, params)

    val errors = form.validate("", params, messages)
    if (errors.isEmpty) {
      successConverter.convert(success(form.read("", params, messages)))
    } else {
      request.set(RequestAttributeErrorsKey, errors)
      errorsConverter.convert(hasErrors(errors))
    }
  }

  protected def validateJson[T, R1, R2](form: ValueType[T])(hasErrors: Seq[(String, String)] => R1, success: T => R2)
    (implicit errorsConverter: ResultConverter[R1], successConverter: ResultConverter[R2]): ActionResult = {
    val map = MutableMap[String, Seq[String]]()
    val json = JsonUtil.deserialize[Map[String, Any]](request.body)
    processJson(json, map, "")

    val params = map.toMap
    request.set(RequestAttributeParamsKey, params)

    val errors = form.validate("", params, messages)
    if (errors.isEmpty) {
      successConverter.convert(success(form.read("", params, messages)))
    } else {
      request.set(RequestAttributeErrorsKey, errors)
      errorsConverter.convert(hasErrors(errors))
    }
  }

  private def processJson(json: Map[String, Any], map: MutableMap[String, Seq[String]], key: String): Unit = {
    json.foreach { case (name, value) =>
      value match {
        case x: String  => addParam(map, key + name, x)
        case x: Int     => addParam(map, key + name, x.toString)
        case x: Long    => addParam(map, key + name, x.toString)
        case x: Boolean => addParam(map, key + name, x.toString)
        case x: List[_] => {
          processJson(x.zipWithIndex.map { case (value, i) => name + "[" + i + "]" -> value }.toMap, map, key)
        }
        case x: Map[String ,Any] @unchecked => {
          processJson(x, map, key + name + ".")
        }
      }
    }
  }

  private def addParam(map: MutableMap[String, Seq[String]], key: String, value: String): Unit = {
    val newValue = map.get(key) match {
      case Some(values) => values :+ value
      case None         => Seq(value)
    }
    map.put(key, newValue)
  }

  implicit object JsonResultConverter extends ResultConverter[Json] {
    def convert(result: Json): ActionResult = {
      ActionResult(
        status = 200,
        body = ByteArrayBody(result.value.getBytes("UTF-8")),
        contentType = "application/json",
        headers = Map.empty
      )
    }
  }

}