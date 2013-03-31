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
 * @version 0.0.2, 2013-03-30
 */
case class OrgHasEmail(pid: Long, eid: Long, pos: Int)

object OrgHasEmail {
  implicit lazy val db = Database.forDataSource(DB.getDataSource())
  val tablename = "OrgHasEmail"
    
  def getAllForOrg(o: Organization): Validation[Throwable, List[OrgHasEmail]] = db withSession {
    try {
      Success(Query(OrgHasEmails).where(_.oid === o.id.get).list)
    } catch {
      case e: Throwable => Failure(e)
    }
  }
}

object OrgHasEmails extends Table[OrgHasEmail](OrgHasEmail.tablename) {
  def oid = column[Long]("oid")
  def eid = column[Long]("eid")
  def pos = column[Int]("position")
  def * = oid ~ eid ~ pos <> (OrgHasEmail.apply _, OrgHasEmail.unapply _)
  def organization = foreignKey("org_fk", oid, Organizations)(_.id)
  def email = foreignKey("email_fk", eid, Emails)(_.id)
  def pk = primaryKey("pk_orghasphone", (oid, eid))
  def update(ohe: OrgHasEmail): Int = OrgHasEmails.where(_.oid === ohe.pid).where(_.eid === ohe.eid).update(ohe)
}