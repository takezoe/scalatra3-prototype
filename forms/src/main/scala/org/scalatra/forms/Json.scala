package org.scalatra.forms

import org.scalatra.util.JsonUtil

object Json {
  def apply[T](value: T, form: ValueType[T]): Json = {
    Json(JsonUtil.serialize(grouping(form.write("", value))))
  }

  def apply(map: Map[String, Any]): Json = {
    Json(JsonUtil.serialize(map))
  }

  private def grouping(values: Seq[(String, Any)]): Map[String, Any] = {
    values.map { case (key, value) =>
      key.split("\\.").toList match {
        case head :: rest => (head, rest, value)
      }
    }.groupBy { case (head, _, _) =>
      head.replaceFirst("\\[[0-9]+\\]", "")
    }.map { case (key, values) =>
      values match {
        case Seq((head, Nil, value)) =>
          if(head.endsWith("]")){
            key -> Seq(value)
          } else {
            key -> value
          }
        case (head, _, _) :: _ =>
          if(head.endsWith("]")){
            val listValues = values.groupBy(_._1).toSeq.sortBy { case (key, _) =>
              val IndexRegex = ".+\\[([0-9]+)\\]$".r
              key match {
                case IndexRegex(index) => index.toLong
              }
            }.map { case (_, subValues) =>
              subValues match {
                case Seq((_, Nil, value)) => value
                case _ => grouping(subValues.map(x => (x._2.mkString("."), x._3)))
              }
            }
            key -> listValues
          } else {
            key -> grouping(values.map(x => (x._2.mkString("."), x._3)))
          }
      }
    }
  }
}

case class Json(value: String)