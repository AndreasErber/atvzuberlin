/**
 *
 */
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
case class OrgHasAddress(oid: Long, aid: Long, pos: Int)

object OrgHasAddress {
  
  implicit lazy val db = Database.forDataSource(DB.getDataSource())
  val tablename = "OrgHasAddress"
    
  def getAllForOrg(o: Organization): Validation[Throwable, List[OrgHasAddress]] = db withSession {
    try {
      Success(Query(OrgHasAddresses).where(_.oid === o.id.get).list)
    } catch {
      case e: Throwable => Failure(e)
    }
  }
}

object OrgHasAddresses extends Table[OrgHasAddress](OrgHasAddress.tablename) {
  
  def oid = column[Long]("oid")
  def aid = column[Long]("aid")
  def pos = column[Int]("position")
  def * = oid ~ aid ~ pos <> (OrgHasAddress.apply _, OrgHasAddress.unapply _)
  def org = foreignKey("org_fk", oid, Organizations)(_.id)
  def address = foreignKey("adr_fk", aid, Addresses)(_.id)
  def pk = primaryKey("pk_orghasaddress", (oid, aid))
  def update(oha: OrgHasAddress): Int = OrgHasAddresses.where(_.oid === oha.oid).where(_.aid === oha.aid).update(oha)
}