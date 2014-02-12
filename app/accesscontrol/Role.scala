/**
 *
 */
package accesscontrol

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
import models.Entity

/**
 * @author andreas
 * @version 0.0.1, 2014-02-07
 */
case class Role(override val id: Option[Long],
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
object Role {

  val tablename = "Role"
}

/**
 * @author andreas
 * @version 0.0.1, 2014-02-07
 */
object Roles extends Table[Role](Role.tablename) {

  lazy val db = Database.forDataSource(DB.getDataSource())

  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name")
  def description = column[String]("description", O.Nullable)
  def created = column[Long]("created")
  def creator = column[String]("creator")
  def modified = column[Long]("modified", O.Nullable)
  def modifier = column[String]("modifier", O.Nullable)
  def * = id.? ~ name ~ description.? ~ created ~ creator ~ modified.? ~ modifier.? <> (Role.apply _, Role.unapply _)

  def withoutId = name ~ description.? ~ created ~ creator ~ modified.? ~ modifier.? returning id
  def insert = (r: Role) => withoutId.insert(r.name, r.description, r.created, r.creator, r.modified, r.modifier)
  def update(r: Role): Int = Roles.where(_.id === r.id).update(r.copy(modified = Some(System.currentTimeMillis())))

  def delete(id: Long): Validation[Throwable, Boolean] = db withSession { s: Session =>
    require(Option(id).isDefined)
    try {
      if (Query(Roles).filter(_.id === id).delete == 1) {
        Success(true)
      } else {
        Failure(new RuntimeException("Failed to delete role with ID " + id))
      }
    } catch {
      case t: Throwable => Failure(t)
    }
  }

  def getAll: Validation[Throwable, List[Role]] = db withSession { s: Session =>
    def q = Query(Roles).sortBy(r => r.name).list
    try {
      Success(q)
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  def load(id: Long): Option[Role] = db withSession { s: Session =>
    require(Option(Long).isDefined)
    Query(Roles).filter(_.id === id).firstOption
  }

  def saveOrUpdate(r: Role): Validation[Throwable, Role] = db withSession { s: Session =>
    require(Option(r).isDefined)

    if (r.id.isDefined) {
      try {
        val count = Roles.update(r)
        if (count > 0) {
          Success(r)
        } else {
          Failure(new RuntimeException("Failed to update role " + r.name))
        }
      } catch {
        case t: Throwable => Failure(t)
      }
    } else {
      try {
        val id = Roles.insert(r)
        Success(r.copy(id = Some(id)))
      } catch {
        case t: Throwable => Failure(t)
      }
    }
  }
}