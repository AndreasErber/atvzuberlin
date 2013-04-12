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
import util.Personal

/**
 * @author andreas
 * @version 0.0.1, 2013-04-12
 */
case class PersonHasHomepage(pid: Long, hid: Long, pos: Int, usage: UsageType = Personal)

object PersonHasHomepage {
  
  implicit lazy val db = Database.forDataSource(DB.getDataSource())
  val tablename = "PersonHasHomepage"
    
  def getAllForPerson(p: Person): Validation[Throwable, List[PersonHasHomepage]] = db withSession {
    try {
      Success(Query(PersonHasHomepages).where(_.pid === p.id.get).list)
    } catch {
      case e: Throwable => Failure(e)
    }
  }
}

object PersonHasHomepages extends Table[PersonHasHomepage](PersonHasHomepage.tablename) {
  
  import scala.slick.lifted.MappedTypeMapper.base
  import scala.slick.lifted.TypeMapper
  implicit val usageTypeMapper: TypeMapper[UsageType] = base[UsageType, Int](ut => ut.id, id => Personal.getUsageType(id).get)

  def pid = column[Long]("pid")
  def hid = column[Long]("hid")
  def pos = column[Int]("position")
  def usage = column[UsageType]("usage")
  def * = pid ~ hid ~ pos ~ usage <> (PersonHasHomepage.apply _, PersonHasHomepage.unapply _)
  def person = foreignKey("person_fk", pid, Persons)(_.id)
  def email = foreignKey("homepage_fk", hid, Homepages)(_.id)
  def pk = primaryKey("pk_personhashomepage", (pid, hid))
  def update(phh: PersonHasHomepage): Int = PersonHasHomepages.where(_.pid === phh.pid).where(_.hid === phh.hid).update(phh)
}