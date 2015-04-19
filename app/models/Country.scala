/**
 *
 */
package models

import play.api.db._
import play.api.Play.current
import scala.slick.driver.PostgresDriver.simple._
import Database.threadLocalSession
import scala.slick.lifted.Query
import scalaz.{Failure, Success, Validation}

/**
 * A helper to provide countries.
 *
 * @author andreas
 * @version 0.0.3, 2015-04-19
 */
case class Country(id: Option[Int],
                   nameDe: Option[String],
                   nameEn: Option[String],
                   name: Option[String],
                   vrc: Option[String],
                   iso2: Option[String],
                   iso3: Option[String],
                   phone: Option[Int],
                   currency: Option[String],
                   tld: Option[String]
                    )

object Country {

  implicit lazy val db = Database.forDataSource(DB.getDataSource())
  val tablename = "Country"

  def load(id: Int): Option[Country] = db withSession {
    Query(Countries).filter(_.id === id).firstOption
  }

  def getAll: Validation[Throwable, List[Country]] = db withSession {
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

  def * = id.? ~ nameDe.? ~ nameEn.? ~ name.? ~ vrc.? ~ iso2.? ~ iso3.? ~ phone.? ~ currency.? ~ tld.? <>(Country.apply _, Country.unapply _)

  def withoutId = nameDe.? ~ nameEn.? ~ name.? ~ vrc.? ~ iso2.? ~ iso3.? ~ phone.? ~ currency.? ~ tld.? returning id

  def insert = (c: Country) => withoutId.insert(c.nameDe, c.nameEn, c.name, c.vrc, c.iso2, c.iso3, c.phone, c.currency, c.tld)

  def update(c: Country): Int = Countries.where(_.id === c.id).update(c)

  def count(): Int = Countries.count()
}