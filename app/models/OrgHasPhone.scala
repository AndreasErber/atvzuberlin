package models

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
 * @version 0.0.1, 2013-03-29
 */
case class OrgHasPhone(pid: Long, phid: Long, pos: Int)

object OrgHasPhone {
  implicit lazy val db = Database.forDataSource(DB.getDataSource())
  val tablename = "OrgHasPhone"
    
  def getAllForOrg(o: Organization): Validation[Throwable, List[OrgHasPhone]] = db withSession {
    try {
      Success(Query(OrgHasPhones).where(_.oid === o.id.get).list)
    } catch {
      case e: Throwable => Failure(e)
    }
  }
}

object OrgHasPhones extends Table[OrgHasPhone](OrgHasPhone.tablename) {
  def oid = column[Long]("oid")
  def phid = column[Long]("phid")
  def pos = column[Int]("position")
  def * = oid ~ phid ~ pos <> (OrgHasPhone.apply _, OrgHasPhone.unapply _)
  def organization = foreignKey("org_fk", oid, Organizations)(_.id)
  def email = foreignKey("phone_fk", phid, Emails)(_.id)
  def pk = primaryKey("pk_orghasphone", (oid, phid))
  def update(ohp: OrgHasPhone): Int = OrgHasPhones.where(_.oid === ohp.pid).where(_.phid === ohp.phid).update(ohp)
}