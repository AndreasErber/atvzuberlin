/**
 *
 */
package models

import java.text.DateFormat
import java.util.Calendar

/**
 * @author andreas
 * @version 0.0.5, 2013-04-13
 */
abstract class Entity(val id: Option[Long] = None, val created: Long = System.currentTimeMillis(), val creator: String, val modified: Option[Long] = None, val modifier: Option[String] = None) extends Equals {

  /**
   * Redefine the inherited method to limit equality to instances of the same type.
   *
   * @param other The instance to compare this instance with.
   * @return <code>true</code> if the other instance is of the same type, <code>false</code> otherwise.
   */
  override def canEqual(other: Any) = other.isInstanceOf[Entity]

  def lastModification(df: DateFormat): String = {
    require(Option(df).isDefined)
    val cal = Calendar.getInstance()
    if (this.modified.isDefined) {
      cal.setTimeInMillis(this.modified.get)
    } else {
      cal.setTimeInMillis(this.created)
    }
    df.format(cal.getTime())
  }
}