/**
 *
 */
package models

import play.api.db._
import play.api.Play.current
import scala.slick.driver.PostgresDriver.simple._
import Database.threadLocalSession
import scala.slick.lifted.Query
import scalaz.{Failure, Success, Validation}
import util.{AtvEvent, EventType}
import java.sql.Timestamp

/**
 * Entity to represent events.
 *
 * @author andreas
 * @version 0.0.3, 2015-04-19
 */
case class Event(override val id: Option[Long] = None,
    title: String,
    description: Option[String],
    start: Timestamp,
    end: Option[Timestamp],
    location: Option[String],
    url: Option[String],
    priority: Int = 2,
    typus: EventType,
    override val created: Long = System.currentTimeMillis(),
    override val creator: String,
    override val modified: Option[Long] = None,
    override val modifier: Option[String] = None) extends Entity(id, created, creator, modified, modifier)

/**
 * Companion object of the [[Event]] case class.
 *
 * @author andreas
 * @version 0.0.3, 2015-04-19
 */
object Event {

  implicit lazy val db = Database.forDataSource(DB.getDataSource())
  val tablename = "Event"
    
  /**
   * Retrieve all [[Event]]s from the persistence store.
   *
   * @return A [[Validation]] with either a [[Throwable]] or a [[List]] of [[Event]]s.
   */
  def getAll: Validation[Throwable, List[Event]] = db withSession {
    try {
      Success(Query(Events).list)
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  /**
   * Retrieve all future [[Event]]s.
   *
   * @param today The latest allowed point in time for an [[Event]] to be included in the result.
   * @return A [[Validation]] with either a [[Throwable]] or a [[List]] of [[Event]]s.
   */
  def getAllUpcoming(today: Timestamp): Validation[Throwable, List[Event]] = db withSession {
    try {
      Success(Query(Events).where(_.start >= today).list)
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  /**
   * Load the [[Event]] related to the given identifier.
   *
   * @param id The identifier of the [[Event]].
   * @return A [[Validation]] with either a [[Throwable]] or an optional [[Event]].
   */
  def load(id: Long): Validation[Throwable, Option[Event]] = db withSession {
    try {
      Success(Query(Events).filter(_.id === id).firstOption)
    } catch {
      case t: Throwable => Failure(t)
    }
  }

  /**
   * Persist a new [[Event]] or update an existing one.
   *
   * @param e The [[Event]] to handle.
   * @return A [[Validation]] with either a [[Throwable]] or an [[Event]].
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
   * Delete the [[Event]] identified by <em>id</em>.
   *
   * @param id Identifier of the [[Event]].
   * @return A [[Validation]] of either a [[Throwable]] or a [[Boolean]] indicating the success of the operation.
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
   * Count the number of [[Event]]s.
   *
   * @return The number of [[Event]]s currently stored in the persistence store.
   */
  def count(): Int = db withSession {
    Events.count()
  }
}

/**
 * Data Access Object for [[Event]]s.
 */
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
  def count(): Int = Events.count()
}