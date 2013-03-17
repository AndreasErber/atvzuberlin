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
 * @version 0.0.2, 2013-03-13
 */
case class Country(val id: Option[Int], 
    val nameDe: Option[String],
    val nameEn: Option[String],
    val name: Option[String],
    val vrc: Option[String],
    val iso2: Option[String],
    val iso3: Option[String],
    val phone: Option[Int],
    val currency: Option[String],
    val tld: Option[String]
)

object Country {
  
  implicit lazy val db = Database.forDataSource(DB.getDataSource())
  val tablename = "Country"
    
  def load(id: Int): Option[Country] = db withSession {
    Query(Countries).filter(_.id === id).firstOption
  }
  
  def getAll(): Validation[Throwable, List[Country]] = db withSession {
    try {
      Success(Query(Countries).list)
    } catch {
      case e: Throwable => Failure(e)
    }
  }
}

object Countries extends Table[Country](Country.tablename) {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
  def nameDe = column[String]("nameDe", O.Nullable)
  def nameEn = column[String]("nameEn", O.Nullable)
  def name = column[String]("name", O.Nullable)
  def vrc = column[String]("vrc", O.Nullable)
  def iso2 = column[String]("iso2", O.Nullable)
  def iso3 = column[String]("iso3", O.Nullable)
  def phone = column[Int]("phone", O.Nullable)
  def currency = column[String]("currency", O.Nullable)
  def tld = column[String]("tld", O.Nullable)
  def * = id.? ~ nameDe.? ~ nameEn.? ~ name.? ~ vrc.? ~ iso2.? ~ iso3.? ~ phone.? ~ currency.? ~ tld.? <> (Country.apply _, Country.unapply _)

  def withoutId = nameDe.? ~ nameEn.? ~ name.? ~ vrc.? ~ iso2.? ~ iso3.? ~ phone.? ~ currency.? ~ tld.? returning id
  def insert = (c: Country) => withoutId.insert(c.nameDe, c.nameEn, c.name, c.vrc, c.iso2, c.iso3, c.phone, c.currency, c.tld)
  def update(c: Country): Int = Countries.where(_.id === c.id).update(c)
  def count(): Int = Countries.count
}