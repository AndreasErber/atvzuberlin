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
 * @verson 0.0.1, 2013-03-29
 */
case class PersonHasPhone(pid: Long, phid: Long, pos: Int, usage: UsageType = Personal, privacy: Privacy = MembersPrivate)

object PersonHasPhone {
  
  implicit lazy val db = Database.forDataSource(DB.getDataSource())
  val tablename = "PersonHasPhone"
    
  def getAllForPerson(p: Person): Validation[Throwable, List[PersonHasPhone]] = db withSession {
    try {
      Success(Query(PersonHasPhones).where(_.pid === p.id.get).list)
    } catch {
      case e: Throwable => Failure(e)
    }
  }
}

object PersonHasPhones extends Table[PersonHasPhone](PersonHasPhone.tablename) {
  
  import scala.slick.lifted.MappedTypeMapper.base
  import scala.slick.lifted.TypeMapper
  implicit val usageTypeMapper: TypeMapper[UsageType] = base[UsageType, Int](ut => ut.id, id => Personal.getUsageType(id).get)
  implicit val privacyMapper: TypeMapper[Privacy] = base[Privacy, Int](p => p.id, id => MembersPrivate.getPrivacy(id).get)

  def pid = column[Long]("pid")
  def phid = column[Long]("phid")
  def pos = column[Int]("position")
  def usage = column[UsageType]("usage")
  def privacy = column[Privacy]("privacy")
  def * = pid ~ phid ~ pos ~ usage ~ privacy <> (PersonHasPhone.apply _, PersonHasPhone.unapply _)
  def person = foreignKey("person_fk", pid, Persons)(_.id)
  def email = foreignKey("phone_fk", phid, Emails)(_.id)
  def pk = primaryKey("pk_personhasphone", (pid, phid))
  def update(php: PersonHasPhone): Int = PersonHasPhones.where(_.pid === php.pid).where(_.phid === php.phid).update(php)
}