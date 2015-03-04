/**
 *
 */
package exception

/**
 * Exception providing information about password errors.
 * 
 * @author andreas
 * @version 0.0.1, 2015-01-10
 */
class PasswordException(message: String, cause: Throwable) extends RuntimeException(message, cause) {

  def this(message: String) {
    this(message, null)
  }

  def this() {
    this(null, null)
  }
}