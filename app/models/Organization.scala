/**
 *
 */
package models

import java.sql.Date
import play.api.db._
import play.api.Play.current
import scala.slick.driver.PostgresDriver.simple._
import Database.threadLocalSession
import scala.slick.lifted.Query
import scalaz.{Failure, Success, Validation}

/**
 * @author andreas
 * @version 0.0.3, 2015-04-19
 */
case class Organization(override val id: Option[Long] = None,
                        name: String,
                        gender: Char,
                        founded: Option[Date],
                        refounded: Option[Date],
                        motto: Option[String],
                        colors: Option[String],
                        city: Option[String],
                        override val created: Long = System.currentTimeMillis(),
                        override val creator: String,
                        override val modified: Option[Long] = None,
                        override val modifier: Option[String] = None) extends Entity(id, created, creator, modified, modifier) {
}

object Organization {

  val tablename = "Organization"
  implicit lazy val db = Database.forDataSource(DB.getDataSource())

  /**
   * Retrieve all organizations from the persistence store.
   */
  def getAll: Validation[Throwable, List[Organization]] = db withSession {
    try {
      Success(Query(Organizations).list)
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  /**
   * Load the organization related to the given identifier.
   */
  def load(id: Long): Validation[Throwable, Option[Organization]] = db withSession {
    try {
      Success(Query(Organizations).filter(_.id === id).firstOption)
    } catch {
      case t: Throwable => Failure(t)
    }
  }

  /**
   * Persist a new organization instance or update an existing one.
   */
  def saveOrUpdate(o: Organization): Validation[Throwable, Organization] = {
    db withSession {

      require(Option(o).isDefined)
      // if the object has an identifier it is an update
      if (o.id.isDefined) {
        try {
          val count = Organizations.update(o)
          if (count == 0) {
            Failure(new RuntimeException("Failed to update organization " + o))
          } else {
            Success(o)
          }
        } catch {
          case e: Throwable => Failure(e)
        }
      } // objects without identifier are new and must be inserted
      else {
        try {
          val id = Organizations.insert(o)
          Success(o.copy(id = Some(id)))
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
      val delCount = Query(Organizations).filter(_.id === id).delete
      if (delCount > 0) Success(true) else Failure(new RuntimeException("Failed to delete organization with id " + id))
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  /**
   * Count the number of occurrences of events.
   */
  def count(): Int = db withSession {
    Organizations.count()
  }
}

object Organizations extends Table[Organization](Organization.tablename) {

  import scala.slick.lifted.MappedTypeMapper.base
  import scala.slick.lifted.TypeMapper

  implicit val CharMapper: TypeMapper[Char] = base[Char, String](d => d.toString, t => t(0))

  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

  def name = column[String]("name", O.NotNull)

  def gender = column[Char]("gender", O.NotNull, O.Default('m'))

  def founded = column[Date]("founded", O.Nullable)

  def refounded = column[Date]("refounded", O.Nullable)

  def motto = column[String]("motto", O.Nullable)

  def colors = column[String]("colors", O.Nullable)

  def city = column[String]("city", O.Nullable)

  def created = column[Long]("created")

  def creator = column[String]("creator")

  def modified = column[Long]("modified", O.Nullable)

  def modifier = column[String]("modifier", O.Nullable)

  def * = id.? ~ name ~ gender ~ founded.? ~ refounded.? ~ motto.? ~ colors.? ~ city.? ~ created ~ creator ~ modified.? ~ modifier.? <>(Organization.apply _, Organization.unapply _)

  def withoutId = name ~ gender ~ founded.? ~ refounded.? ~ motto.? ~ colors.? ~ city.? ~ created ~ creator ~ modified.? ~ modifier.? returning id

  def insert = (o: Organization) => withoutId.insert(o.name, o.gender, o.founded, o.refounded, o.motto, o.colors, o.city, o.created, o.creator, o.modified, o.modifier)

  def update(o: Organization): Int = Organizations.where(_.id === o.id).update(o.copy(modified = Some(System.currentTimeMillis())))

  def count(): Int = Organizations.count()
}