package org.scalatra.util

import java.util.UUID

import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec
import org.apache.commons.codec.binary.Hex

object CryptUtils {

  val SecretKey = Option(System.getProperty("org.scalatra.secretKey")).getOrElse(UUID.randomUUID().toString)

  def crypt(content: String): String = {
    val cipher = Cipher.getInstance("AES")
    val keySpec = new SecretKeySpec(SecretKey.substring(0, 16).getBytes("UTF-8"), "AES")
    cipher.init(Cipher.ENCRYPT_MODE, keySpec)
    val bytes = cipher.doFinal(content.getBytes("UTF-8"))

    Hex.encodeHexString(bytes)
  }

  def decrypt(content: String): String = {
    try {
      val decoded = Hex.decodeHex(content)

      val cipher = Cipher.getInstance("AES")
      val keySpec = new SecretKeySpec(SecretKey.substring(0, 16).getBytes("UTF-8"), "AES")
      cipher.init(Cipher.DECRYPT_MODE, keySpec)
      val bytes = cipher.doFinal(decoded)

      new String(bytes, "UTF-8")
    } catch {
      case e: Exception =>
        e.printStackTrace()
        ""
    }
  }
}