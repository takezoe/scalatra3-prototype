package org.scalatra

import org.apache.commons.fileupload.FileItem
import org.apache.commons.fileupload.disk.DiskFileItemFactory
import org.apache.commons.fileupload.servlet.ServletFileUpload
import scala.collection.JavaConverters._

object FileUploadSupport {
  private val FileParamsRequestKey: String = "org.scalatra.FileUploadSupport.fileParams"
  private val FileMultiParamsRequestKey: String = "org.scalatra.FileUploadSupport.fileMultiParams"
}

trait FileUploadSupport { this: ScalatraBase =>
  import FileUploadSupport._

  protected def fileParams: Map[String, FileItem] = {
    request.get(FileParamsRequestKey).getOrElse {
      val fileParams = fileMultiParams.map { case (name, values) => name -> values.head }
      request.set(FileParamsRequestKey, fileParams)
      fileParams
    }.asInstanceOf[Map[String, FileItem]]
  }

  protected def fileMultiParams: Map[String, Seq[FileItem]] = {
    if (isMultipartRequest(request)){
      request.get(FileMultiParamsRequestKey).getOrElse {
        val factory    = new DiskFileItemFactory()
        val fileUpload = new ServletFileUpload(factory)
        val fileItems  = fileUpload.parseRequest(request.underlying)

        val fileMultiParams = fileItems.asScala.map { fileItem =>
          fileItem.getFieldName -> fileItem
        }.groupBy { case (name, fileItem) =>
          name
        }.map { case (name, fileItems) =>
          name -> fileItems.map { case (name, fileItem) => fileItem }
        }
        request.set(FileMultiParamsRequestKey, fileMultiParams)
        fileMultiParams
      }.asInstanceOf[Map[String, Seq[FileItem]]]
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
