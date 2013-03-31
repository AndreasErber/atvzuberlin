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

/**
 * @author andreas
 * @version 0.0.4, 2013-03-29
 */
case class PersonHasAddress(pid: Long, aid: Long, pos: Int, usage: UsageType = Personal, privacy: Privacy = MembersPrivate) 

object PersonHasAddress {
  
  implicit lazy val db = Database.forDataSource(DB.getDataSource())
  val tablename = "PersonHasAddress"
    
  def getAllForPerson(p: Person): Validation[Throwable, List[PersonHasAddress]] = db withSession {
    try {
      Success(Query(PersonHasAddresses).where(_.pid === p.id.get).list)
    } catch {
      case e: Throwable => Failure(e)
    }
  }

}

object PersonHasAddresses extends Table[PersonHasAddress](PersonHasAddress.tablename) {

  import scala.slick.lifted.MappedTypeMapper.base
  import scala.slick.lifted.TypeMapper
//  implicit val personMapper: TypeMapper[Person] = base[Person, Long](p => p.id.get, id => Person.load(id).get)
//  implicit val addressMapper: TypeMapper[Address] = base[Address, Long](a => a.id.get, id => Address.load(id).get)
  implicit val usageTypeMapper: TypeMapper[UsageType] = base[UsageType, Int](ut => ut.id, id => Personal.getUsageType(id).get)
  implicit val privacyMapper: TypeMapper[Privacy] = base[Privacy, Int](p => p.id, id => MembersPrivate.getPrivacy(id).get)

  def pid = column[Long]("pid")
  def aid = column[Long]("aid")
  def pos = column[Int]("position")
  def usage = column[UsageType]("usage")
  def privacy = column[Privacy]("privacy")
  def * = pid ~ aid ~ pos ~ usage ~ privacy  <> (PersonHasAddress.apply _, PersonHasAddress.unapply _)
  def person = foreignKey("person_fk", pid, Persons)(_.id)
  def address = foreignKey("adr_fk", aid, Addresses)(_.id)
  def pk = primaryKey("pk_personhasaddress", (pid, aid))
  def update(pha: PersonHasAddress): Int = PersonHasAddresses.where(_.pid === pha.pid).where(_.aid === pha.aid).update(pha)
}
