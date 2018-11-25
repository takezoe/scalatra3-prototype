package org.scalatra.forms

import org.scalatra.i18n._
import org.scalatra.{ActionResult, ResultConverter, ScalatraBase}

object FormSupport {
  private val RequestAttributeParamsKey = "org.scalatra.forms.FormSupport.params"
  private val RequestAttributeErrorsKey = "org.scalatra.forms.FormSupport.errors"
}

trait FormSupport { self: ScalatraBase with I18nSupport =>
  import FormSupport._

  protected def validate[T, R1, R2](form: ValueType[T])(hasErrors: Seq[(String, String)] => R1, success: T => R2)
    (implicit errorsConverter: ResultConverter[R1], successConverter: ResultConverter[R2]): ActionResult = {
    val params = multiParams
    request.set(RequestAttributeParamsKey, params)

    val errors = form.validate("", params, messages)
    if (errors.isEmpty) {
      successConverter.convert(success(form.convert("", params, messages)))
    } else {
      request.set(RequestAttributeErrorsKey, errors)
      errorsConverter.convert(hasErrors(errors))
    }
  }

}