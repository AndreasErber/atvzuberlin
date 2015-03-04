/**
 *
 */
package util

import java.security.MessageDigest

/**
 * Utilities for encryption.
 * 
 * @author andreas
 * @version 0.0.1, 2015-01-11
 */
object EncryptionUtil {

  /**
   * Generate a password hash using the SHA-256 algorithm.
   * 
   * @see http://blog.knoldus.com/2014/03/18/password-encryption-in-play/
   */
  def encrypt(password: String): String = {
    val algorithm = MessageDigest.getInstance("SHA-256")
    val passwordBytes = password.getBytes
    algorithm.reset()
    algorithm.update(passwordBytes)
    val messageDigest = algorithm.digest()
    this.getHexString(messageDigest)
  }

  /**
   * Generate a hex string from the given <em>messageDigest</em>
   * 
   * @see http://blog.knoldus.com/2014/03/18/password-encryption-in-play/
   */
  private def getHexString(messageDigest: Array[Byte]): String = {
    val hexString = new StringBuffer
    messageDigest foreach { digest =>
      val hex = Integer.toHexString(0xFF & digest)
      if (hex.length() == 1)
        hexString.append('0')
      else
        hexString.append(hex)
    }
    hexString.toString()
  }
}