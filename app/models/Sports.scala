/**
 *
 */
package models

import play.api.db._
import play.api.Play.current
import scala.slick.driver.PostgresDriver.simple._
import Database.threadLocalSession
import scala.slick.lifted.Query
import scalaz.Validation
import scalaz.Failure
import scalaz.Success

/**
 * Entity to represent a sports.
 *
 * @author andreas
 * @version 0.0.3, 2015-04-20
 */
case class Sports(override val id: Option[Long],
                  title: String,
                  description: Option[String],
                  override val created: Long = System.currentTimeMillis(),
                  override val creator: String,
                  override val modified: Option[Long] = None,
                  override val modifier: Option[String]) extends Entity(id, created, creator, modified, modifier) {

  /**
   * Redefine the string representation of this class.
   *
   * @return The string representation of this instance.
   */
  override def toString: String = {

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

  /**
   * Retrieve all sports from the persistence store.
   */
  def getAll: Validation[Throwable, List[Sports]] = db withSession {
    def q = Query(Sportss).sortBy(n => n.title).list
    try {
      Success(q)
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  /**
   * Load the sports related to the given identifier.
   */
  def load(id: Long): Validation[Throwable, Option[Sports]] = db withSession {
    try {
      Success(Query(Sportss).filter(_.id === id).firstOption)
    } catch {
      case t: Throwable => Failure(t)
    }
  }

  /**
   * Persist a new sports or update an existing one.
   */
  def saveOrUpdate(s: Sports): Validation[Throwable, Sports] = {
    db withSession {

      require(Option(s).isDefined)
      // if the object has an identifier it is an update
      if (s.id.isDefined) {
        try {
          val count = Sportss.update(s)
          if (count == 0) {
            Failure(new RuntimeException("Failed to update sports " + s))
          } else {
            Success(s)
          }
        } catch {
          case e: Throwable => Failure(e)
        }
      } // objects without identifier are new and must be inserted
      else {
        try {
          val id = Sportss.insert(s)
          Success(s.copy(id = Some(id)))
        } catch {
          case e: Throwable => Failure(e)
        }
      }
    }
  }

  /**
   * Delete the sports identified by id.
   */
  def delete(id: Long): Validation[Throwable, Boolean] = db withSession {
    try {
      val delCount = Query(Sportss).filter(_.id === id).delete
      if (delCount > 0) Success(true) else Failure(new RuntimeException("Failed to delete sports with id " + id))
    } catch {
      case e: Throwable => Failure(e)
    }
  }
}

/**
 * @author andreas
 * @version 0.0.1, 2013-06-30
 */
object Sportss extends Table[Sports](Sports.tablename) {

  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

  def title = column[String]("title")

  def description = column[String]("description", O.Nullable, O.DBType("text"))

  def created = column[Long]("created")

  def creator = column[String]("creator")

  def modified = column[Long]("modified", O.Nullable)

  def modifier = column[String]("modifier", O.Nullable)

  def * = id.? ~ title ~ description.? ~ created ~ creator ~ modified.? ~ modifier.? <>(Sports.apply _, Sports.unapply _)

  def withoutId = title ~ description.? ~ created ~ creator ~ modified.? ~ modifier.? returning id

  def insert = (s: Sports) => withoutId.insert(s.title, s.description, s.created, s.creator, s.modified, s.modifier)

  def update(s: Sports): Int = Sportss.where(_.id === s.id).update(s.copy(modified = Some(System.currentTimeMillis())))
}
