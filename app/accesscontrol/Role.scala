/**
 *
 */
package accesscontrol

import db.GenericDao
import play.api.db._
import play.api.Play.current
import scala.slick.driver.PostgresDriver.simple._
import scala.slick.lifted.Query
import Database.threadLocalSession
import scalaz.Failure
import scalaz.Success
import scalaz.Validation
import models.Entity
import scalaz.Failure

/**
 * @author aer
 * @version 0.0.1, 2014-05-27
 */
case class Role(override val id: Option[Long] = None,
  val name: String,
  val description: Option[String] = None,
  override val created: Long = System.currentTimeMillis(),
  override val creator: String,
  override val modified: Option[Long] = None,
  override val modifier: Option[String] = None) extends Entity(id, created, creator, modified, modifier) {

  require(Option(name).isDefined)
}

object Roles extends Table[Role]("Role") with GenericDao[Role] {

  override def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name")
  def description = column[String]("description", O.Nullable)
  def created = column[Long]("created")
  def creator = column[String]("creator")
  def modified = column[Long]("modified", O.Nullable)
  def modifier = column[String]("modifier", O.Nullable)

  def * = id.? ~ name ~ description.? ~ created ~ creator ~ modified.? ~ modifier.? <> (Role.apply _, Role.unapply _)

  def withoutId = name ~ description.? ~ created ~ creator ~ modified.? ~ modifier.? returning id

  def insert = db withSession { (r: Role) => withoutId.insert(r.name, r.description, r.created, r.creator, r.modified, r.modifier) }
  override def update(r: Role): Int = db withSession { Roles.where(_.id === r.id).update(r.copy(modified = Some(System.currentTimeMillis()))) }

  def saveOrUpdate(r: Role): Validation[Throwable, Role] = db withSession {
    if (r.id.isDefined) {
      val pUpd = r.copy(modified = Some(System.currentTimeMillis()))
      try {
        val upd = this.update(pUpd)
        if (upd > 0) {
          Success(pUpd)
        } else {
          Failure(new RuntimeException("Failed to update privilege " + upd))
        }
      } catch {
        case e: Throwable => Failure(e)
      }
    } else {
      try {
        val id = this.insert(r)
        Success(r.copy(id = Some(id)))
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }

  def getSome(ids: List[Long]): Validation[Throwable, List[Role]] = db withSession {
    if (ids.isEmpty) {
      return Success(Nil);
    }
    
    try {
      val q = for {
        r <- Roles
        if r.id inSetBind ids
      } yield r
      Success(q.list)
    } catch {
      case e: Throwable => Failure(e)
    }
  }
  
  def getByName(name: String): Validation[Throwable, Option[Role]] = db withSession {
    try {
      Success(Query(Roles).where(_.name === name).firstOption)
    } catch {
      case t: Throwable => Failure(t)
    }
    
  }
}
