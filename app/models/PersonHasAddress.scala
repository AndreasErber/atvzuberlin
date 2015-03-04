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
import util.UsageType
import util.Privacy
import util.Personal
import util.MembersPrivate
import db.GenericDao

/**
 * Associations between {@link Person}s and {@link Address}es.
 *
 * @author andreas
 * @version 0.0.5, 2015-01-05
 */
case class PersonHasAddress(pid: Long, aid: Long, pos: Int, usage: UsageType = Personal, privacy: Privacy = MembersPrivate)

object PersonHasAddress {

  implicit lazy val db = Database.forDataSource(DB.getDataSource())
  val tablename = "PersonHasAddress"

  

}

object PersonHasAddresses extends Table[PersonHasAddress](PersonHasAddress.tablename) {


  import scala.slick.lifted.MappedTypeMapper.base
  import scala.slick.lifted.TypeMapper
  implicit val usageTypeMapper = base[UsageType, Int](ut => ut.id, id => Personal.getUsageType(id).get)
  implicit val privacyMapper = base[Privacy, Int](p => p.id, id => MembersPrivate.getPrivacy(id).get)
  implicit lazy val db = Database.forDataSource(DB.getDataSource())

  def pid = column[Long]("pid")
  def aid = column[Long]("aid")
  def pos = column[Int]("position")
  def usage = column[UsageType]("usage")
  def privacy = column[Privacy]("privacy")
  def * = pid ~ aid ~ pos ~ usage ~ privacy <> (PersonHasAddress.apply _, PersonHasAddress.unapply _)
  def person = foreignKey("person_fk", pid, Persons)(_.id)
  def address = foreignKey("adr_fk", aid, Addresses)(_.id)
  def pk = primaryKey("pk_personhasaddress", (pid, aid))

  def getAllForPerson(p: Person): Validation[Throwable, List[PersonHasAddress]] = db withSession {
    try {
      Success(Query(PersonHasAddresses).where(_.pid === p.id.get).list)
    } catch {
      case e: Throwable => Failure(e)
    }
  }
  
  /**
   * Update the given {@link PersonHasAddress} association.
   * 
   * @param pha The {@link PersonHasAddress} association to be updated.
   * @returns A {@link Validation} that holds the number of rows affected or the {@link Throwable} in case of error.
   */
  def update(pha: PersonHasAddress): Validation[Throwable, Int] = db withSession {
    try {
      Success(PersonHasAddresses.where(_.pid === pha.pid).where(_.aid === pha.aid).update(pha))
    } catch {
      case t: Throwable => Failure(t)
    }
  }

  /**
   * Delete the association between a {@link Person} identified by <em>pid</em> and an {@link Address} specified be
   * <em>aid</em>.
   *
   * @param pid Identifier of the {@link Person} involved.
   * @param aid Identifier of the {@link Address} involved.
   * @returns A {@link Validation} that holds the number of rows affected or the {@link Throwable} in case of error.
   */
  def delete(pid: Long, aid: Long): Validation[Throwable, Int] = db withSession {
    try {
      Success(Query(PersonHasAddresses).where(_.pid === pid).where(_.aid === aid).delete)
    } catch {
      case t: Throwable => Failure(t)
    }
  }
}
