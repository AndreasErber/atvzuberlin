/**
 *
 */
package models

import util.Division
import java.sql.Date
import db.GenericDao
import scala.slick.driver.PostgresDriver.simple._
import Database.threadLocalSession
import scalaz.Failure
import scalaz.Success
import scalaz.Validation

/**
 * An entity providing a relation between a [[Person]] and a [[Charge]].
 *
 * @author andreas
 * @version 0.0.2, 2015-04-18
 */
case class PersonInCharge(override val id: Option[Long],
  person: Person,
  charge: Charge,
  division: Division.Division = Division.Aktivitas,
  start: Date,
  end: Date,
  override val created: Long = System.currentTimeMillis(),
  override val creator: String,
  override val modified: Option[Long] = None,
  override val modifier: Option[String]) extends Entity(id, created, creator, modified, modifier) {
}

object PersonInCharges extends Table[PersonInCharge]("PersonInCharge") with GenericDao[PersonInCharge] {

  import scala.slick.lifted.MappedTypeMapper.base
  import scala.slick.lifted.TypeMapper
  implicit val divisionMapper: TypeMapper[Division.Division] = base[Division.Division, String](d => d.toString, string => Division.withName(string))
  implicit val personMapper: TypeMapper[Person] = base[Person, Long](p => p.id.get, id => Person.load(id).get)
  implicit val chargeMapper: TypeMapper[Charge] = base[Charge, Long](c => c.id.get, id => Charge.load(id).toOption
    .get.get)

  override def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def person = column[Person]("person")
  def charge = column[Charge]("charge")
  def division = column[Division.Division]("division")
  def start = column[Date]("start")
  def end = column[Date]("end")
  def created = column[Long]("created")
  def creator = column[String]("creator")
  def modified = column[Long]("modified", O.Nullable)
  def modifier = column[String]("modifier", O.Nullable)

  def * = id.? ~ person ~ charge ~ division ~ start ~ end ~ created ~ creator ~ modified.? ~ modifier.? <> (PersonInCharge.apply _, PersonInCharge.unapply _)
  def withoutId = person ~ charge ~ division ~ start ~ end ~ created ~ creator ~ modified.? ~ modifier.? returning id

  def insert = db withSession { (c: PersonInCharge) => withoutId.insert(c.person, c.charge, c.division, c.start, c.end, c.created, c.creator, c.modified, c.modifier) }
  override def update(c: PersonInCharge): Int = db withSession { PersonInCharges.where(_.id === c.id).update(c.copy(modified = Some(System.currentTimeMillis()))) }

  /**
   * Persist a [[PersonInCharge]] item in the data store.
   *
   * If the specified instance is new, e.g., has no identifier set, it gets inserted into the database otherwise the existing entry with the same ID gets updated.
   */
  def saveOrUpdate(pic: PersonInCharge): Validation[Throwable, PersonInCharge] = db withSession {
    if (pic.id.isDefined) {
      val picUpd = pic.copy(modified = Some(System.currentTimeMillis()))
      try {
        val upd = this.update(picUpd)
        if (upd > 0) {
          Success(picUpd)
        } else {
          Failure(new RuntimeException("Failed to update person in charge " + upd))
        }
      } catch {
        case e: Throwable => Failure(e)
      }
    } else {
      try {
        val id = this.insert(pic)
        Success(pic.copy(id = Some(id)))
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }
}