/**
 *
 */
package accesscontrol

import models.Entity
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
 * @version 0.0.1, 2014-02-07
 */
case class Privilege(override val id: Option[Long],
  val name: String,
  val description: Option[String],
  override val created: Long = System.currentTimeMillis(),
  override val creator: String,
  override val modified: Option[Long] = None,
  override val modifier: Option[String]) extends Entity(id, created, creator, modified, modifier)

/**
 * @author andreas
 * @version 0.0.1, 2014-02-07
 */
object Privilege {

  implicit val db = Database.forDataSource(DB.getDataSource())
  val tablename = "Privilege"

  private def delete(id: Long): Validation[Throwable, Boolean] = {
    try {
      val delCount = Query(Privileges).filter(_.id === id).delete
      if (delCount > 0) Success(true) else Failure(new RuntimeException("Failed to delete privilege with id " + id))
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  def getAll(): Validation[Throwable, List[Privilege]] = db withSession {
    def q = Query(Privileges).sortBy(p => p.name).list
    try {
      Success(q)
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  def load(id: Long): Option[Privilege] = db withSession {
    Query(Privileges).filter(_.id === id).firstOption
  }

  def saveOrUpdate(p: Privilege): Validation[Throwable, Privilege] = {
    db withSession {

      require(Option(p).isDefined)
      // if the object has an identifier it is an update
      if (p.id.isDefined) {
        try {
          val count = Privileges.update(p)
          if (count == 0) {
            Failure(new RuntimeException("Failed to update privilege " + p))
          } else {
            Success(p)
          }
        } catch {
          case e: Throwable => Failure(e)
        }
      } // objects without identifier are new and must be inserted
      else {
        try {
          val id = Privileges.insert(p)
          Success(p.copy(id = Some(id)))
        } catch {
          case e: Throwable => Failure(e)
        }
      }
    }
  }
}

/**
 * @author andreas
 * @version 0.0.1, 2014-02-07
 */
object Privileges extends Table[Privilege](Privilege.tablename) {

  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name")
  def description = column[String]("description", O.Nullable)
  def created = column[Long]("created")
  def creator = column[String]("creator")
  def modified = column[Long]("modified", O.Nullable)
  def modifier = column[String]("modifier", O.Nullable)
  def * = id.? ~ name ~ description.? ~ created ~ creator ~ modified.? ~ modifier.? <> (Privilege.apply _, Privilege.unapply _)

  def withoutId = name ~ description.? ~ created ~ creator ~ modified.? ~ modifier.? returning id
  def insert = (p: Privilege) => withoutId.insert(p.name, p.description, p.created, p.creator, p.modified, p.modifier)
  def update(p: Privilege): Int = Privileges.where(_.id === p.id).update(p.copy(modified = Some(System.currentTimeMillis())))
}