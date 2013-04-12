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
 * @version 0.0.1, 2013-04-12
 */
case class OrgHasHomepage(oid: Long, hid: Long, pos: Int)

object OrgHasHomepage {
  implicit lazy val db = Database.forDataSource(DB.getDataSource())
  val tablename = "OrgHasHomepage"
    
  def getAllForOrg(o: Organization): Validation[Throwable, List[OrgHasHomepage]] = db withSession {
    try {
      Success(Query(OrgHasHomepages).where(_.oid === o.id.get).list)
    } catch {
      case e: Throwable => Failure(e)
    }
  }
}

object OrgHasHomepages extends Table[OrgHasHomepage](OrgHasHomepage.tablename) {
  def oid = column[Long]("oid")
  def hid = column[Long]("hid")
  def pos = column[Int]("position")
  def * = oid ~ hid ~ pos <> (OrgHasHomepage.apply _, OrgHasHomepage.unapply _)
  def organization = foreignKey("org_fk", oid, Organizations)(_.id)
  def email = foreignKey("homepage_fk", hid, Homepages)(_.id)
  def pk = primaryKey("pk_orghashomepage", (oid, hid))
  def update(ohh: OrgHasHomepage): Int = OrgHasHomepages.where(_.oid === ohh.oid).where(_.hid === ohh.hid).update(ohh)
}