package org.scalatra.forms

import org.scalatra.i18n._
import org.scalatra.ScalatraBase

trait FormSupport { self: ScalatraBase with I18nSupport =>

  protected def validate[T](form: ValueType[T])(hasErrors: Seq[(String, String)] => Any, success: T => Any): Any = {
    val params = multiParams
    request.setAttribute(RequestAttributeParamsKey, params)

    val errors = form.validate("", params, messages)
    if (errors.isEmpty) {
      success(form.convert("", params, messages))
    } else {
      request.setAttribute(RequestAttributeErrorsKey, errors)
      hasErrors(errors)
    }
  }

}