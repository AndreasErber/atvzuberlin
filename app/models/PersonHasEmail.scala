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
 * @verson 0.0.2, 2013-03-30
 */
case class PersonHasEmail(pid: Long, eid: Long, pos: Int, usage: UsageType = Personal, privacy: Privacy = MembersPrivate)

object PersonHasEmail {
  
  implicit lazy val db = Database.forDataSource(DB.getDataSource())
  val tablename = "PersonHasEmail"
    
  def getAllForPerson(p: Person): Validation[Throwable, List[PersonHasEmail]] = db withSession {
    try {
      Success(Query(PersonHasEmails).where(_.pid === p.id.get).list)
    } catch {
      case e: Throwable => Failure(e)
    }
  }
}

object PersonHasEmails extends Table[PersonHasEmail](PersonHasEmail.tablename) {
  
  import scala.slick.lifted.MappedTypeMapper.base
  import scala.slick.lifted.TypeMapper
  implicit val usageTypeMapper: TypeMapper[UsageType] = base[UsageType, Int](ut => ut.id, id => Personal.getUsageType(id).get)
  implicit val privacyMapper: TypeMapper[Privacy] = base[Privacy, Int](p => p.id, id => MembersPrivate.getPrivacy(id).get)

  def pid = column[Long]("pid")
  def eid = column[Long]("eid")
  def pos = column[Int]("position")
  def usage = column[UsageType]("usage")
  def privacy = column[Privacy]("privacy")
  def * = pid ~ eid ~ pos ~ usage ~ privacy <> (PersonHasEmail.apply _, PersonHasEmail.unapply _)
  def person = foreignKey("person_fk", pid, Persons)(_.id)
  def email = foreignKey("email_fk", eid, Emails)(_.id)
  def pk = primaryKey("pk_personhasemail", (pid, eid))
  def update(phe: PersonHasEmail): Int = PersonHasEmails.where(_.pid === phe.pid).where(_.eid === phe.eid).update(phe)
}