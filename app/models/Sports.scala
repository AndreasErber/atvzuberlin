/**
 *
 */
package models

import play.data.validation.Constraints
import play.api.db._
import play.api.Logger
import play.api.Play.current
import scala.slick.driver.PostgresDriver.simple._
import Database.threadLocalSession
import scala.slick.lifted.Parameters
import scala.slick.lifted.Query
import scalaz.Validation
import scalaz.Failure
import scalaz.Success

/**
 * @author andreas
 * @version 0.0.1, 2013-06-30
 */
case class Sports(override val id: Option[Long],
  val title: String,
  val description: Option[String],
  override val created: Long = System.currentTimeMillis(),
  override val creator: String,
  override val modified: Option[Long] = None,
  override val modifier: Option[String]) extends Entity(id, created, creator, modified, modifier) {

  /**
   * Redefine the string representation of this class.
   *
   * @return The string representation of this instance.
   */
  override def toString(): String = {

    var result = ""
    if (id.isDefined) {
      result = "(" + id.get + ") "
    }
    result + title + ", " + description
  }

  /**
   * Redefine the inherited method to limit equality to instances of the same type.
   *
   * @param other The instance to compare this instance with.
   * @return <code>true</code> if the other instance is of the same type, <code>false</code> otherwise.
   */
  override def canEqual(other: Any) = other.isInstanceOf[Sports]

  /**
   * Redefine the comparison function.
   *
   * @param other The instance to compare to this instance.
   * @return <code>true</code> if the instance are the same or equal, <code>false</code> otherwise.
   */
  override def equals(other: Any): Boolean = other match {
    case that: Sports =>
      if (this eq that) true
      else {
        (that.## == this.##) &&
          (that canEqual this) &&
          (this.title == that.title) &&
          (this.description == that.description)
      }
    case _ => false
  }

  /**
   * Redefine the hashing function.
   *
   * @return The hash of this object.
   */
  override def hashCode(): Int = {

    var hash = 17
    hash = 31 * hash + title.hashCode
    hash = 31 * hash + description.hashCode
    hash
  }
}

object Sports {

  implicit val db = Database.forDataSource(DB.getDataSource())
  val tablename = "Sports"
}

/**
 * @author andreas
 * @version 0.0.1, 2013-06-30
 */
object Sportss extends Table[Sports](Sports.tablename) {

  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def title = column[String]("title")
  def description = column[String]("description", O.Nullable)
  def created = column[Long]("created")
  def creator = column[String]("creator")
  def modified = column[Long]("modified", O.Nullable)
  def modifier = column[String]("modifier", O.Nullable)
  def * = id.? ~ title ~ description.? ~ created ~ creator ~ modified.? ~ modifier.? <> (Sports.apply _, Sports.unapply _)

  def withoutId = title ~ description.? ~ created ~ creator ~ modified.? ~ modifier.? returning id
  def insert = (s: Sports) => withoutId.insert(s.title, s.description, s.created, s.creator, s.modified, s.modifier)
  def update(s: Sports): Int = Sportss.where(_.id === s.id).update(s.copy(modified = Some(System.currentTimeMillis())))
}
