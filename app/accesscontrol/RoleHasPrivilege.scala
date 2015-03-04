/**
 *
 */
package accesscontrol

import db.GenericDao
import play.api.db._
import play.api.Play.current
import scala.slick.driver.PostgresDriver.simple._
import scala.slick.lifted.{ Query, TypeMapper }
import scala.slick.lifted.MappedTypeMapper.base
import Database.threadLocalSession
import scalaz.{ Failure, Success, Validation }
import models.Entity
import play.Logger

/**
 * Relation between a {@link Role} and the {@link Privilege}s it is assigned to.
 * 
 * @author andreas
 * @version 0.0.3, 2015-01-04
 */
case class RoleHasPrivilege(override val id: Option[Long],
  val role: Role,
  val privilege: Privilege,
  override val created: Long = System.currentTimeMillis(),
  override val creator: String,
  override val modified: Option[Long] = None,
  override val modifier: Option[String]) extends Entity(id, created, creator, modified, modifier) {
}

object RoleHasPrivileges extends Table[RoleHasPrivilege]("RoleHasPrivilege") with GenericDao[RoleHasPrivilege] {

  import scala.slick.lifted.MappedTypeMapper.base
  import scala.slick.lifted.TypeMapper
  implicit val roleMapper = base[Role, Long](r => r.id.get, id => Roles.get(id).get)
  implicit val privilegeMapper = base[Privilege, Long](p => p.id.get, id => Privileges.get(id).get)

  override def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def role = column[Role]("role")
  def privilege = column[Privilege]("privilege")
  def created = column[Long]("created")
  def creator = column[String]("creator")
  def modified = column[Long]("modified", O.Nullable)
  def modifier = column[String]("modifier", O.Nullable)

  def * = id.? ~ role ~ privilege ~ created ~ creator ~ modified.? ~ modifier.? <> (RoleHasPrivilege.apply _, RoleHasPrivilege.unapply _)

  def withoutId = role ~ privilege ~ created ~ creator ~ modified.? ~ modifier.? returning id

  def insert = db withSession {
    (c: RoleHasPrivilege) => withoutId.insert(c.role, c.privilege, c.created, c.creator, c.modified, c.modifier)
  }
  override def update(c: RoleHasPrivilege): Int = db withSession {
    RoleHasPrivileges.where(_.id === c.id).update(c.copy(modified = Some(System.currentTimeMillis())))
  }

  /**
   * Save or update the given relation <em>rhp</em>.
   *
   * @param rhp The relation to update.
   * @return a {@link Validation} that either indicates success or failure of the operation.
   */
  def saveOrUpdate(rhp: RoleHasPrivilege): Validation[Throwable, RoleHasPrivilege] = db withSession {
    if (rhp.id.isDefined) {

      Logger.debug(s"Updating relation role to privilege with ID ${rhp.id.get}")

      val rhpUpd = rhp.copy(modified = Some(System.currentTimeMillis()))
      try {
        val upd = this.update(rhpUpd)
        if (upd > 0) {
          Success(rhpUpd)
        } else {
          Failure(new RuntimeException("Failed to update roleHasPrivilege " + upd))
        }
      } catch {
        case e: Throwable => Failure(e)
      }
    } else {

      Logger.debug(s"Inserting new relation from role '${rhp.role.name}' to privilege '${rhp.privilege.name}'")

      try {
        val id = this.insert(rhp)
        Logger.debug(s"Successfully created new relation with ID $id.")
        Success(rhp.copy(id = Some(id)))
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }

  /**
   * Retrieve the relations for the given <em>roles</em>.
   * 
   * @param roles A list of {@link Role}s to retrieve the corresponding relations for.
   * @return a {@link Validation} that either indicates success or failure of the operation.
   */
  def getSome(roles: List[Role]): Validation[Throwable, List[RoleHasPrivilege]] = db withSession {
    if (roles.isEmpty) {
      Logger.info("Getting privileges for roles: No roles provided.")
      return Success(Nil);
    }
    
    Logger.debug("Getting privileges for roles: " + roles.map(r => r.name).mkString(","))

    try {
      val q = for {
        r <- RoleHasPrivileges
        if r.role inSetBind roles
      } yield r
      Logger.debug("Query: " + q.selectStatement)
      val result = q.list
      Logger.debug(" ... found " + result.size + " result set entries:")
//      for (item <- result) {
//        Logger.debug(item.privilege.name)
//      }
      Success(result)
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  def deleteByPrivilege(privilege: Privilege) = db withSession {
    this.where(_.privilege === privilege).delete
  }
}