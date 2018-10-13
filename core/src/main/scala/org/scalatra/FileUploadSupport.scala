package org.scalatra

import java.io.{FileOutputStream, InputStream}
import java.nio.file.Files

import javax.mail.internet.MimeMultipart
import javax.mail.util.ByteArrayDataSource
import org.apache.commons.io.IOUtils

trait FileUploadSupport {
  self: ScalatraBase =>

  private val ParamNamePattern = "form-data; name=\"(.+?)\".*".r
  private val FileNamePattern = ".+filename=\"(.+?)\"".r

  private val FileMultiParamsRequestKey = "org.scalatra.FileUploadSupport.fileMultiParams"

  protected def fileParams: Map[String, FileParam] = {
    fileMultiParams.map { case (name, values) => name -> values.head }
  }

  protected def fileMultiParams: Map[String, Seq[FileParam]] = {
    request.get(FileMultiParamsRequestKey).getOrElse {
      val multi = new MimeMultipart(new ByteArrayDataSource(request.inputStream, "multipart/form-data"))

      val params = (0 to multi.getCount - 1).flatMap { i =>
        val message = multi.getBodyPart(i)
        val e = message.getAllHeaders

        var paramName: Option[String] = None
        var fileName: Option[String]  = None

        while(e.hasMoreElements){
          val header = e.nextElement().asInstanceOf[javax.mail.Header]
          if(header.getName.toUpperCase() == "CONTENT-DISPOSITION"){
            paramName = header.getValue match {
              case ParamNamePattern(name) => Some(name)
              case _ => None
            }
            fileName = header.getValue match {
              case FileNamePattern(name) => Some(name)
              case _ => None
            }
          }
        }

        paramName.map { paramName =>
          message.getContent match {
            case s: String =>
              paramName -> new FileParam(paramName, new String(s.getBytes("ISO-8859-1"), "UTF-8"), None)
            case i: InputStream =>
              val tempFile = Files.createTempFile("scalatra-", ".upload") // TODO Delete temp file?
              IOUtils.copy(i, new FileOutputStream(tempFile.toFile))
              paramName -> new FileParam(paramName, fileName.getOrElse(""), Some(tempFile.toFile))
          }
        }
      }

      val fileMultiParams = params
        .groupBy { case (name, param) => name }
        .map     { case (name, params) => name -> params.map(_._2) }

      request.set(FileMultiParamsRequestKey, fileMultiParams)
      fileMultiParams

    }.asInstanceOf[Map[String, Seq[FileParam]]]
  }

}

class FileParam(val name: String, val value: String, val file: Option[java.io.File]) {

}