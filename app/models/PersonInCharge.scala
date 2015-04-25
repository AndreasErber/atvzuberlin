/**
 *
 */
package models

import java.util.Calendar

import util.Division
import java.sql.Date
import db.GenericDao
import scala.slick.driver.PostgresDriver.simple._
import scala.slick.jdbc.GetResult
import scala.slick.jdbc.StaticQuery.interpolation
import Database.threadLocalSession
import scalaz.Failure
import scalaz.Success
import scalaz.Validation

/**
 * An entity providing a relation between a [[Person]] and a [[Charge]].
 *
 * @author andreas
 * @version 0.0.4, 2015-04-24
 */
case class PersonInCharge(override val id: Option[Long],
                          person: Person,
                          charge: Charge,
                          start: Date,
                          end: Option[Date],
                          override val created: Long = System.currentTimeMillis(),
                          override val creator: String,
                          override val modified: Option[Long] = None,
                          override val modifier: Option[String]) extends Entity(id, created, creator, modified, modifier) {
}

object PersonInCharges extends Table[PersonInCharge]("PersonInCharge") with GenericDao[PersonInCharge] {

  import scala.slick.lifted.MappedTypeMapper.base

  implicit val divisionMapper = base[Division.Division, String](d => d.toString, string => Division.withName(string))
  implicit val personMapper = base[Person, Long](p => p.id.get, id => Person.load(id).toOption.get.get)
  implicit val chargeMapper = base[Charge, Long](c => c.id.get, id => Charge.load(id).toOption.get.get)

//  implicit val getPersonInChargeResult = GetResult(r => PersonInCharge(r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r
//    .<<, r.<<))

  override def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

  def person = column[Person]("person")

  def charge = column[Charge]("charge")

  def division = column[Division.Division]("division")

  def start = column[Date]("start")

  def end = column[Date]("end", O.Nullable)

  def created = column[Long]("created")

  def creator = column[String]("creator")

  def modified = column[Long]("modified", O.Nullable)

  def modifier = column[String]("modifier", O.Nullable)

  def * = id.? ~ person ~ charge ~ start ~ end.? ~ created ~ creator ~ modified.? ~ modifier.? <>(PersonInCharge.apply
    _, PersonInCharge.unapply _)

  def withoutId = person ~ charge ~ start ~ end.? ~ created ~ creator ~ modified.? ~ modifier.? returning id

  def insert = db withSession { (c: PersonInCharge) => withoutId.insert(c.person, c.charge, c.start, c.end, c.created, c.creator, c.modified, c.modifier) }

  override def update(c: PersonInCharge): Int = db withSession {
    PersonInCharges.where(_.id === c.id).update(c.copy(modified = Some(System.currentTimeMillis())))
  }

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

  /**
   * Retrieve all [[PersonInCharge]] items for the given <em>division</em>.
   *
   * @param division The division to get all [[PersonInCharge]] items for.
   * @return The corresponding [[List]] of [[PersonInCharge]] items.
   */
  def getAllByDivision(division: String) : Validation[Throwable, List[PersonInCharge]] = db withSession {
    try {
      val query = for {
         pic <- PersonInCharges
         c <- Charges
         if pic.charge.asColumnOf[Long] === c.id
         if c.division.asColumnOf[String] === division
       } yield pic
      Success(query.list())
    }
    catch {
      case t: Throwable => Failure(t)
    }
  }

  def getAllCurrent: Validation[Throwable, List[PersonInCharge]] = db withSession {
    val cal = Calendar.getInstance()
    try {
      val query = Query(this).filter(pic => pic.end.isNull || (pic.end >= new Date(cal.getTimeInMillis)))
      Success(query.list())
    }
    catch {
      case t: Throwable => Failure(t)
    }
  }

  def getAllCurrentByDivision(division: String): Validation[Throwable, List[PersonInCharge]] = db withSession {
    val cal = Calendar.getInstance()
    try {
      val query = for {
        pic <- PersonInCharges
        c <- Charges
        if pic.end.isNull || pic.end >= new Date(cal.getTimeInMillis)
        if pic.charge.asColumnOf[Long] === c.id
        if c.division.asColumnOf[String] === division
      } yield pic
      Success(query.list())
    }
    catch {
      case t: Throwable => Failure(t)
    }
  }
}