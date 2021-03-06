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

/**
 * Entity to describe an [[Enrollment]] to an event.
 * 
 * @author andreas
 * @version 0.0.5, 2015-04-19
 */
case class Enrollment(override val id: Option[Long],
  event: Long,
  person: Long,
  numberOfAdults: Int = 1,
  numberOfKids: Int = 0,
  confirmed: Boolean = false,
  cancelled: Boolean = false,
  override val created: Long = System.currentTimeMillis(),
  override val creator: String,
  override val modified: Option[Long] = None,
  override val modifier: Option[String]) extends Entity(id, created, creator, modified, modifier) {

  /**
   * Redefine the inherited method to limit equality to instances of the same type.
   *
   * @param other The instance to compare this instance with.
   * @return <code>true</code> if the other instance is of the same type, <code>false</code> otherwise.
   */
  override def canEqual(other: Any) = other.isInstanceOf[Enrollment]

  /**
   * Redefine the comparison function.
   *
   * @param other The instance to compare to this instance.
   * @return <code>true</code> if the instance are the same or equal, <code>false</code> otherwise.
   */
  override def equals(other: Any): Boolean = other match {
    case that: Enrollment =>
      if (this eq that) true
      else {
        (that.## == this.##) &&
          (that canEqual this) &&
          (this.event == that.event) &&
          (this.person == that.person) &&
          (this.numberOfAdults == that.numberOfAdults) &&
          (this.numberOfKids == that.numberOfKids) &&
          (this.confirmed == that.confirmed) &&
          (this.cancelled == that.cancelled)
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
    hash = 31 * hash + event.hashCode
    hash = 31 * hash + person.hashCode
    hash = 31 * hash + numberOfAdults
    hash = 31 * hash + numberOfKids
    hash = 31 * hash + confirmed.hashCode
    hash = 31 * hash + cancelled.hashCode
    hash
  }
}

/**
 * @author andreas
 * @version 0.0.1, 2013-04-28
 */
object Enrollment {

  implicit val db = Database.forDataSource(DB.getDataSource())
  val tablename = "Enrollment"

  /**
   * Remove the [[Enrollment]] with the ID <em>id</em>.
   *
   * @param id The identifier of the [[Enrollment]] to be removed.
   * @return <code>true</code> if the removal was successful, <code>false</code> otherwise.
   */
  def delete(id: Long): Validation[Throwable, Boolean] = db withSession {
    try {
      val delCount = Query(Enrollments).filter(_.id === id).delete
      if (delCount > 0) Success(true) else Failure(new RuntimeException("Failed to delete enrollment with id " + id))
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  /**
   * Load the [[Enrollment]] with the ID <em>id</em>
   * 
   * @param id The identifier of the [[Enrollment]] to be loaded.
   * @return The [[Enrollment]] corresponding to the identifier, is any, or a [[Failure]].
   */
  def load(id: Long): Validation[Throwable, Option[Enrollment]] = db withSession {
    try {
      Success(Query(Enrollments).filter(_.id === id).firstOption)
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  /**
   * Load all [[Enrollment]]s for a given [[Event]] identified by <em>eid</em>.
   * 
   * @param eid The identifier of the [[Event]] to load the [[Enrollment]]s for.
   * @return The possibly empty list of [[Enrollment]]s connected to the specified [[Event]].
   */
  def loadByEvent(eid: Long): Validation[Throwable, List[(Enrollment, Person)]] = db withSession {
    try {
      val join = for {
        e <- Enrollments if e.event === eid
        p <- Persons if e.person === p.id 
      } yield (e, p)
      Success(join.list)
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  /**
   * Load all pending [[Enrollment]]s for a given [[Person]] identified by <em>pid</em>, i.e., all [[Enrollment]]s 
   * to [[Event]] that have not taken place yet.
   * 
   * @param pid The identifier of the [[Person]] to load the [[Enrollment]]s for.
   * @return The possibly empty list of [[Enrollment]]s connected to the specified [[Person]].
   */
  def loadByPerson(pid: Long): Validation[Throwable, List[(Enrollment, Event)]] = db withSession {
    try {
      val join = for {
        e <- Enrollments if e.person === pid
        ev <- Events if e.event === ev.id
      } yield (e, ev)
      Success(join.list)
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  /**
   * Save a new [[Enrollment]] or update an existing one. The concrete action will be decided upon the 
   * presence of an identifier of the [[Enrollment]].
   * 
   * @param e The [[Enrollment]] to be persisted.
   * @return The same [[Enrollment]] as specified as parameter. In case of a save action, the returned
   * instance will have its id field set.
   */
  def saveOrUpdate(e: Enrollment): Validation[Throwable, Enrollment] = {
    db withSession {
      if (e.id.isDefined) {
        try {
          val count = Enrollments.update(e)
          if (count == 0) {
            Failure(new RuntimeException("Failed to update enrollment " + e))
          } else {
            Success(e)
          }
        } catch {
          case e: Throwable => Failure(e)
        }
      } else {
        try {
          val id = Enrollments.insert(e)
          Success(e.copy(id = Some(id)))
        } catch {
          case e: Throwable => Failure(e)
        }
      }
    }
  }
}

/**
 * @author andreas
 * @version 0.0.1, 2013-04-28
 */
object Enrollments extends Table[Enrollment](Enrollment.tablename) {

  import scala.slick.lifted.MappedTypeMapper.base
  import scala.slick.lifted.TypeMapper
  implicit val eventMapper: TypeMapper[Event] = base[Event, Long](e => e.id.get, id => Event.load(id).toOption.get.get)
  implicit val personMapper: TypeMapper[Person] = base[Person, Long](p => p.id.get, id => Person.load(id).toOption.get.get)

  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def event = column[Long]("event")
  def person = column[Long]("person")
  def numberOfAdults = column[Int]("numberOfAdults")
  def numberOfKids = column[Int]("numberOfKids")
  def confirmed = column[Boolean]("confirmed")
  def cancelled = column[Boolean]("cancelled")
  def created = column[Long]("created")
  def creator = column[String]("creator")
  def modified = column[Long]("modified", O.Nullable)
  def modifier = column[String]("modifier", O.Nullable)
  def * = id.? ~ event ~ person ~ numberOfAdults ~ numberOfKids ~ confirmed ~ cancelled ~ created ~ creator ~ modified.? ~ modifier.? <> (Enrollment.apply _, Enrollment.unapply _)

  def withoutId = event ~ person ~ numberOfAdults ~ numberOfKids ~ confirmed ~ cancelled ~ created ~ creator ~ modified.? ~ modifier.? returning id
  def insert = (e: Enrollment) => withoutId.insert(e.event, e.person, e.numberOfAdults, e.numberOfKids, e.confirmed, e.cancelled, e.created, e.creator, e.modified, e.modifier)
  def update(e: Enrollment): Int = Enrollments.where(_.id === e.id).update(e.copy(modified = Some(System.currentTimeMillis())))
}