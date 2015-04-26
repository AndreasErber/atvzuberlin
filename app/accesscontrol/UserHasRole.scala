/**
 *
 */
package accesscontrol

import db.GenericDao
import scala.slick.driver.PostgresDriver.simple._
import scala.slick.lifted.Query
import Database.threadLocalSession
import scalaz.{ Failure, Success, Validation }
import models.Entity
import play.Logger

/**
 * Entity representing the relationship between a [[User]] and a [[Role]].
 *
 * @author andreas
 * @version 0.0.3, 2015-04-26
 */
case class UserHasRole(override val id: Option[Long],
  user: User,
  role: Role,
  override val created: Long = System.currentTimeMillis(),
  override val creator: String,
  override val modified: Option[Long] = None,
  override val modifier: Option[String]) extends Entity(id, created, creator, modified, modifier) {
}

object UserHasRoles extends Table[UserHasRole]("UserHasRole") with GenericDao[UserHasRole] {

  import scala.slick.lifted.MappedTypeMapper.base

  implicit val userMapper = base[User, String](_.username, username => User.findByName(username).toOption.get.get)
  implicit val roleMapper = base[Role, Long](_.id.get, id => Roles.get(id).toOption.get.get)

  override def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def user = column[User]("user")
  def role = column[Role]("role")
  def created = column[Long]("created")
  def creator = column[String]("creator")
  def modified = column[Long]("modified", O.Nullable)
  def modifier = column[String]("modifier", O.Nullable)

  def * = id.? ~ user ~ role ~ created ~ creator ~ modified.? ~ modifier.? <> (UserHasRole.apply _, UserHasRole.unapply _)

  def withoutId = user ~ role ~ created ~ creator ~ modified.? ~ modifier.? returning id
  

  def getByUser(u: User): Validation[Throwable, List[UserHasRole]] = db withSession {
    Logger.debug("Getting userHasRoles by user instance.")
    try {
      val q = Query(this).filter(_.user === u)
      Logger.debug("Query: " + q.selectStatement)
      val result = q.list
      Logger.debug(" ... found " + result.size + " result set entries.")
      Success(result)
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  def getByUser(un: String): Validation[Throwable, List[UserHasRole]] = db withSession {
    Logger.debug("Getting userHasRoles by username string.")
    try {
      val userV = User.findByName(un)
      userV match {
        case Success(userOp) => userOp match  {
          case Some(user) => getByUser(user)
          case None => throw new RuntimeException(s"Failed to find a user with username '$un'")
        }
        case Failure(t) => throw t
      }
    } catch {
      case e: Throwable => Failure(e)
    }
  }
  
  def insert = db withSession { (uhr: UserHasRole) =>
    withoutId.insert(uhr.user, uhr.role, uhr.created, uhr.creator, uhr.modified, uhr.modifier)
  }

  override def update(uhr: UserHasRole): Int = db withSession {
    UserHasRoles.where(_.id === uhr.id).update(uhr.copy(modified = Some(System.currentTimeMillis())))
  }

  def saveOrUpdate(uhr: UserHasRole): Validation[Throwable, UserHasRole] = db withSession {
    if (uhr.id.isDefined) {
      val uhrUpd = uhr.copy(modified = Some(System.currentTimeMillis()))
      try {
        val upd = this.update(uhrUpd)
        if (upd > 0) {
          Success(uhrUpd)
        } else {
          Failure(new RuntimeException("Failed to update UserHasRole " + upd))
        }
      } catch {
        case e: Throwable => Failure(e)
      }
    } else {
      try {
        val id = this.insert(uhr)
        Success(uhr.copy(id = Some(id)))
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }
}