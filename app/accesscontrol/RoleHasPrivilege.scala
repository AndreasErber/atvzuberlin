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

/**
 * @author andreas
 * @version 0.0.1, 2014-02-07
 */
case class RoleHasPrivilege(roleId: Long, privId: Long)

/**
 * @author andreas
 * @version 0.0.1, 2014-02-07
 */
object RoleHasPrivilege {
  val tablename = "RoleHasPrivilege"
}

/**
 * @author andreas
 * @version 0.0.1, 2014-02-07
 */
object RoleHasPrivileges extends Table[RoleHasPrivilege](RoleHasPrivilege.tablename) {

  lazy val db = Database.forDataSource(DB.getDataSource())

  def roleId = column[Long]("roleId")
  def privId = column[Long]("privId")
  def * = roleId ~ privId <> (RoleHasPrivilege.apply _, RoleHasPrivilege.unapply _)
  def role = foreignKey("role_fk", roleId, Roles)(_.id)
  def priv = foreignKey("priv_fk", privId, Privileges)(_.id)
  def pk = primaryKey("pk_rolehasprivilege", (roleId, privId))

  def getAllForRole(roleId: Long): Validation[Throwable, List[RoleHasPrivilege]] = db withSession { s: Session =>
    try {
      Success(Query(RoleHasPrivileges).where(_.roleId === roleId).list)
    } catch {
      case t: Throwable => Failure(t)
    }
  }
}