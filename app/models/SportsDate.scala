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

import java.sql.Time

/**
 * @author andreas
 * @version 0.0.1, 2013-06-30
 */
case class SportsDate(override val id: Option[Long],
  val sports: Long,
  val locationName: Option[String],
  val locationStreet: Option[String],
  val locationZip: Option[String],
  val locationCity: Option[String] = Some("Berlin"),
  val weekday: Option[String],
  val start: Option[Time],
  val end: Option[Time],
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
    result + sports + ", " + locationName + ", " + locationStreet + ", " +
      locationZip + ", " + locationCity + ", " + weekday + ", " + start + ", " + end
  }

  /**
   * Redefine the inherited method to limit equality to instances of the same type.
   *
   * @param other The instance to compare this instance with.
   * @return <code>true</code> if the other instance is of the same type, <code>false</code> otherwise.
   */
  override def canEqual(other: Any) = other.isInstanceOf[SportsDate]

  /**
   * Redefine the comparison function.
   *
   * @param other The instance to compare to this instance.
   * @return <code>true</code> if the instance are the same or equal, <code>false</code> otherwise.
   */
  override def equals(other: Any): Boolean = other match {
    case that: SportsDate =>
      if (this eq that) true
      else {
        (that.## == this.##) &&
          (that canEqual this) &&
          (this.sports == that.sports) &&
          (this.locationName == that.locationName) &&
          (this.locationStreet == that.locationStreet) &&
          (this.locationZip == that.locationZip) &&
          (this.locationCity == that.locationCity) &&
          (this.weekday == that.weekday) &&
          (this.start == that.start) &&
          (this.end == that.end)
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
    hash = 31 * hash + sports.hashCode
    hash = 31 * hash + locationName.hashCode
    hash = 31 * hash + locationStreet.hashCode
    hash = 31 * hash + locationZip.hashCode
    hash = 31 * hash + locationCity.hashCode
    hash = 31 * hash + weekday.hashCode
    hash = 31 * hash + start.hashCode
    hash = 31 * hash + end.hashCode
    hash
  }
}

object SportsDate {

  implicit val db = Database.forDataSource(DB.getDataSource())
  val tablename = "SportsDate"

  /**
   * Retrieve all sports dates from the persistence store.
   */
  def getAll(): Validation[Throwable, List[SportsDate]] = db withSession {
    def q = Query(SportsDates).sortBy(sd => sd.sports).list
    try {
      Success(q)
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  /**
   * Load the sports dates related to the given identifier.
   */
  def load(id: Long): Option[SportsDate] = db withSession {
    Query(SportsDates).filter(_.id === id).firstOption
  }

  /**
   * Persist a new sports date or update an existing one.
   */
  def saveOrUpdate(s: SportsDate): Validation[Throwable, SportsDate] = {
    db withSession {

      require(Option(s).isDefined)
      // if the object has an identifier it is an update
      if (s.id.isDefined) {
        try {
          val count = SportsDates.update(s)
          if (count == 0) {
            Failure(new RuntimeException("Failed to update sports date " + s))
          } else {
            Success(s)
          }
        } catch {
          case e: Throwable => Failure(e)
        }
      } // objects without identifier are new and must be inserted
      else {
        try {
          val id = SportsDates.insert(s)
          Success(s.copy(id = Some(id)))
        } catch {
          case e: Throwable => Failure(e)
        }
      }
    }
  }

  /**
   * Delete the sports date identified by id.
   */
  def delete(id: Long): Validation[Throwable, Boolean] = db withSession {
    try {
      val delCount = Query(SportsDates).filter(_.id === id).delete
      if (delCount > 0) Success(true) else Failure(new RuntimeException("Failed to delete sports date with id " + id))
    } catch {
      case e: Throwable => Failure(e)
    }
  }

}

/**
 * @author andreas
 * @version 0.0.1, 2013-06-30
 */
object SportsDates extends Table[SportsDate](SportsDate.tablename) {

  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def sports = column[Long]("sports")
  def locationName = column[String]("locationName", O.Nullable)
  def locationStreet = column[String]("locationStreet", O.Nullable)
  def locationZip = column[String]("locationZip", O.Nullable)
  def locationCity = column[String]("locationCity", O.Nullable)
  def weekday = column[String]("weekday", O.Nullable)
  def start = column[Time]("start", O.Nullable)
  def end = column[Time]("end", O.Nullable)
  def created = column[Long]("created")
  def creator = column[String]("creator")
  def modified = column[Long]("modified", O.Nullable)
  def modifier = column[String]("modifier", O.Nullable)
  def * = id.? ~ sports ~ locationName.? ~ locationStreet.? ~ locationZip.? ~ locationCity.? ~ weekday.? ~ start.? ~ end.? ~ created ~ creator ~ modified.? ~ modifier.? <> (SportsDate.apply _, SportsDate.unapply _)

  def withoutId = sports ~ locationName.? ~ locationStreet.? ~ locationZip.? ~ locationCity.? ~ weekday.? ~ start.? ~ end.? ~ created ~ creator ~ modified.? ~ modifier.? returning id
  def insert = (sd: SportsDate) => withoutId.insert(sd.sports, sd.locationName, sd.locationStreet, sd.locationZip, sd.locationCity, sd.weekday, sd.start, sd.end, sd.created, sd.creator, sd.modified, sd.modifier)
  def update(sd: SportsDate): Int = SportsDates.where(_.id === sd.id).update(sd.copy(modified = Some(System.currentTimeMillis())))
}
