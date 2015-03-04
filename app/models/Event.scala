/**
 *
 */
package models

import java.sql.Date
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
import util.EventType
import util.AtvEvent
import java.sql.Timestamp

/**
 * @author andreas
 * @version 0.0.2, 2013-03-10
 */
case class Event(override val id: Option[Long] = None,
    val title: String,
    val description: Option[String],
    val start: Timestamp,
    val end: Option[Timestamp],
    val location: Option[String],
    val url: Option[String],
    val priority: Int = 2,
    val typus: EventType,
    override val created: Long = System.currentTimeMillis(),
    override val creator: String,
    override val modified: Option[Long] = None,
    override val modifier: Option[String] = None) extends Entity(id, created, creator, modified, modifier)

object Event {

  implicit lazy val db = Database.forDataSource(DB.getDataSource())
  val tablename = "Event"
    
  /**
   * Retrieve all events from the persistence store.
   */
  def getAll(): Validation[Throwable, List[Event]] = db withSession {
    try {
      Success(Query(Events).list)
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  def getAllUpcoming(today: Timestamp): Validation[Throwable, List[Event]] = db withSession {
    try {
      Success(Query(Events).where(_.start >= today).list)
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  /**
   * Load the event related to the given identifier.
   */
  def load(id: Long): Option[Event] = db withSession {
    Query(Events).filter(_.id === id).firstOption
  }

  /**
   * Persist a new event instance or update an existing one.
   */
  def saveOrUpdate(e: Event): Validation[Throwable, Event] = {
    db withSession {

      require(Option(e).isDefined)
      // if the object has an identifier it is an update
      if (e.id.isDefined) {
        try {
          val count = Events.update(e)
          if (count == 0) {
            Failure(new RuntimeException("Failed to update event " + e))
          } else {
            Success(e)
          }
        } catch {
          case e: Throwable => Failure(e)
        }
      } // objects without identifier are new and must be inserted
      else {
        try {
          val id = Events.insert(e)
          Success(e.copy(id = Some(id)))
        } catch {
          case e: Throwable => Failure(e)
        }
      }
    }
  }

  /**
   * Delete the event identified by id.
   */
  def delete(id: Long): Validation[Throwable, Boolean] = db withSession {
    try {
      val delCount = Query(Events).filter(_.id === id).delete
      if (delCount > 0) Success(true) else Failure(new RuntimeException("Failed to delete event with id " + id))
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  /**
   * Count the number of occurrences of events.
   */
  def count(): Int = db withSession {
    Events.count
  }
}

object Events extends Table[Event](Event.tablename) {

  import scala.slick.lifted.MappedTypeMapper.base
  import scala.slick.lifted.TypeMapper

  implicit val EventTypeMapper: TypeMapper[EventType] = base[EventType, Int](et => et.id, t => AtvEvent.getEventType(t).get)

  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def title = column[String]("title")
  def description = column[String]("description", O.Nullable)
  def start = column[Timestamp]("start")
  def end = column[Timestamp]("end", O.Nullable)
  def location = column[String]("location", O.Nullable)
  def url = column[String]("url", O.Nullable)
  def priority = column[Int]("priority", O.Default(2))
  def typus = column[EventType]("typus")
  def created = column[Long]("created")
  def creator = column[String]("creator")
  def modified = column[Long]("modified", O.Nullable)
  def modifier = column[String]("modifier", O.Nullable)
  def * = id.? ~ title ~ description.? ~ start ~ end.? ~ location.? ~ url.? ~ priority ~ typus ~ created ~ creator ~ modified.? ~ modifier.? <> (Event.apply _, Event.unapply _)
  
  def withoutId = title ~ description.? ~ start ~ end.? ~ location.? ~ url.? ~ priority ~ typus ~ created ~ creator ~ modified.? ~ modifier.? returning id
  def insert = (e: Event) => withoutId.insert(e.title, e.description, e.start, e.end, e.location, e.url, e.priority, e.typus, e.created, e.creator, e.modified, e.modifier)
  def update(e: Event): Int = Events.where(_.id === e.id).update(e.copy(modified = Some(System.currentTimeMillis())))
  def count(): Int = Events.count
}