package org.scalatra

import org.apache.commons.fileupload.FileItem
import org.apache.commons.fileupload.disk.DiskFileItemFactory
import org.apache.commons.fileupload.servlet.ServletFileUpload
import scala.collection.JavaConverters._

trait FileUploadSupport { this: ScalatraBase =>

  lazy val fileParams: Map[String, FileItem] = {
    fileMultiParams.map { case (name, values) => name -> values.head }
  }

  lazy val fileMultiParams: Map[String, Seq[FileItem]] = {
    if (isMultipartRequest(request)){
      val factory    = new DiskFileItemFactory()
      val fileUpload = new ServletFileUpload(factory)
      val fileItems  = fileUpload.parseRequest(request.underlying)

      fileItems.asScala.map { fileItem =>
        fileItem.getFieldName -> fileItem
      }.groupBy { case (name, fileItem) =>
        name
      }.map { case (name, fileItems) =>
        name -> fileItems.map { case (name, fileItem) => fileItem }
      }
    } else Map.empty
  }

  private def isMultipartRequest(req: ScalatraRequest): Boolean = {
    val isPostOrPut = Set(Method.Post, Method.Put, Method.Patch).exists(_.matches(req.underlying.getMethod))
    isPostOrPut && (req.contentType match {
      case Some(contentType) => contentType.startsWith("multipart/")
      case _ => false
    })
  }

}
