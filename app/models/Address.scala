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
import util.Privacy
import util.MembersPrivate

/**
 * @author andreas
 * @version 0.0.2, 2013-03-19
 */
case class Address(override val id: Option[Long] = None,
  val addon: Option[String],
  val street: Option[String],
  val postbox: Option[String],
  val city: String,
  val zip: String,
  val country: Country,
  val usage: UsageType = Personal,
  val privacy: Privacy = MembersPrivate,
  override val created: Long = System.currentTimeMillis(),
  override val creator: String,
  override val modified: Option[Long] = None,
  override val modifier: Option[String] = None) extends Entity(id, created, creator, modified, modifier)

object Address {

  implicit lazy val db = Database.forDataSource(DB.getDataSource())
  val tablename = "Address"

  def getAll(): Validation[Throwable, List[Address]] = db withSession {
    try {
      Success(Query(Addresses).list)
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  /**
   * Get the list of Email items an Organization has.
   *
   * Note, that the output list is sorted according to the position parameter in the relation table.
   */
  def getOrgAddresses(o: Organization): Validation[Throwable, List[Address]] = {
    require(o.id.isDefined)
    db withSession {
      try {
        val result = for {
          oha <- OrgHasAddress.sortBy(x => (x.oid, x.pos)) if oha.oid === o.id.get
          a <- Addresses if oha.aid === a.id
        } yield (a)
        Success(result.list)
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }

  /**
   * Get the list of Email items a Person has.
   *
   * Note, that the output list is sorted according to the position parameter in the relation table.
   */
  def getPersonAddresses(p: Person): Validation[Throwable, List[Address]] = {
    require(p.id.isDefined)
    db withSession {
      try {
        val result = for {
          pha <- PersonHasAddress.sortBy(x => (x.pid, x.pos)) if pha.pid === p.id.get
          a <- Addresses if pha.aid === a.id
        } yield (a)
        Success(result.list)
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }

  /**
   * Load the Address related to the given identifier.
   */
  def load(id: Long): Option[Address] = db withSession {
    Query(Addresses).filter(_.id === id).firstOption
  }

  /**
   * Insert the specified Address item in the database and set it into a relation to the given Organization.
   *
   * If the Organization is not yet persisted it will be in a first step.
   */
  def saveOrgAddress(o: Organization, a: Address): Validation[Throwable, Address] = {

    var o1 = o
    if (!o.id.isDefined) {
      val result = Organization.saveOrUpdate(o)
      if (result.isSuccess) {
        o1 = result.toOption.get
      } else {
        return Failure(result.fail.toOption.get)
      }
    }
    db withSession {
      try {
        val isUpdate = if (a.id.isDefined) true else false
        val result1 = Address.saveOrUpdate(a)
        if (result1.isSuccess) {
          if (!isUpdate) {
            val l = Query(OrgHasAddress).where(_.oid === o1.id.get).list
            OrgHasAddress.insert((o1.id.get, result1.toOption.get.id.get, l.length + 1));
          }
          result1
        } else
          Failure(result1.fail.toOption.get)
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }

  /**
   * Insert the specified Address item in the database and set it into a relation to the given Person.
   *
   * If the person is not yet persisted it will be in a first step.
   */
  def savePersonAddress(p: Person, a: Address): Validation[Throwable, Address] = {

    var p1 = p
    if (!p.id.isDefined) {
      val result = Person.saveOrUpdate(p)
      if (result.isSuccess) {
        p1 = Person.saveOrUpdate(p).toOption.get
      } else {
        return Failure(result.fail.toOption.get)
      }
    }
    db withSession {
      try {
        val isUpdate = if (a.id.isDefined) true else false
        val result1 = Address.saveOrUpdate(a)
        if (result1.isSuccess) {
          if (!isUpdate) {
            val l = Query(PersonHasAddress).where(_.pid === p1.id.get).list
            PersonHasAddress.insert((p1.id.get, result1.toOption.get.id.get, l.length + 1));
          }
          result1
        } else
          Failure(result1.fail.toOption.get)
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }

  /**
   * Persist a new Address instance or update an existing one.
   */
  def saveOrUpdate(a: Address): Validation[Throwable, Address] = {
    db withSession {

      require(Option(a).isDefined)
      // if the object has an identifier it is an update
      if (a.id.isDefined) {
        try {
          val count = Addresses.update(a)
          if (count == 0) {
            Failure(new RuntimeException("Failed to update address " + a))
          } else {
            Success(a)
          }
        } catch {
          case e: Throwable => Failure(e)
        }
      } // objects without identifier are new and must be inserted
      else {
        try {
          val id = Addresses.insert(a)
          Success(a.copy(id = Some(id)))
        } catch {
          case e: Throwable => Failure(e)
        }
      }
    }
  }

  /**
   * Delete the address identified by id.
   */
  def deletePersonAddress(pid: Long, id: Long): Validation[Throwable, Boolean] = db withSession {
    val del = Query(PersonHasAddress).where(_.pid === pid).where(_.aid === id).delete
    if (del > 0) {
      try {
        val delCount = Query(Addresses).filter(_.id === id).delete
        if (delCount > 0) Success(true) else Failure(new RuntimeException("Failed to delete address with id " + id))
      } catch {
        case e: Throwable => Failure(e)
      }
    } else {
      Failure(new RuntimeException("Failed to delete the connection between the person and the address. (" + pid + ", " + id + ")"))
    }
  }

  /**
   * Count the number of occurrences of addresses.
   */
  def count(): Int = db withSession {
    Addresses.count
  }
}

object Addresses extends Table[Address](Address.tablename) {

  import scala.slick.lifted.MappedTypeMapper.base
  import scala.slick.lifted.TypeMapper
  implicit val countryMapper: TypeMapper[Country] = base[Country, Int](c => c.id.get, id => Country.load(id).get)
  implicit val usageTypeMapper: TypeMapper[UsageType] = base[UsageType, Int](ut => ut.id, id => Personal.getUsageType(id).get)
  implicit val privacyMapper: TypeMapper[Privacy] = base[Privacy, Int](p => p.id, id => MembersPrivate.getPrivacy(id).get)

  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def addon = column[String]("addon", O.Nullable)
  def street = column[String]("street", O.Nullable)
  def postbox = column[String]("postbox", O.Nullable)
  def city = column[String]("city")
  def zip = column[String]("zip")
  def country = column[Country]("country")
  def usage = column[UsageType]("usage")
  def privacy = column[Privacy]("privacy")
  def created = column[Long]("created")
  def creator = column[String]("creator")
  def modified = column[Long]("modified", O.Nullable)
  def modifier = column[String]("modifier", O.Nullable)
  def * = id.? ~ addon.? ~ street.? ~ postbox.? ~ city ~ zip ~ country ~ usage ~ privacy ~ created ~ creator ~ modified.? ~ modifier.? <> (Address.apply _, Address.unapply _)

  def withoutId = addon.? ~ street.? ~ postbox.? ~ city ~ zip ~ country ~ usage ~ privacy ~ created ~ creator ~ modified.? ~ modifier.? returning id
  def insert = (a: Address) => withoutId.insert(a.addon, a.street, a.postbox, a.city, a.zip, a.country, a.usage, a.privacy, a.created, a.creator, a.modified, a.modifier)
  def update(a: Address): Int = Addresses.where(_.id === a.id).update(a.copy(modified = Some(System.currentTimeMillis())))
  def count(): Int = Addresses.count
}

object PersonHasAddress extends Table[(Long, Long, Int)]("PersonHasAddress") {
  def pid = column[Long]("pid")
  def aid = column[Long]("aid")
  def pos = column[Int]("position")
  def * = pid ~ aid ~ pos
  def person = foreignKey("person_fk", pid, Persons)(_.id)
  def address = foreignKey("adr_fk", aid, Addresses)(_.id)
  def pk = primaryKey("pk_personhasaddress", (pid, aid))
}

object OrgHasAddress extends Table[(Long, Long, Int)]("OrgHasAddress") {
  def oid = column[Long]("oid")
  def aid = column[Long]("aid")
  def pos = column[Int]("position")
  def * = oid ~ aid ~ pos
  def org = foreignKey("org_fk", oid, Organizations)(_.id)
  def address = foreignKey("adr_fk", aid, Addresses)(_.id)
  def pk = primaryKey("pk_orghasaddress", (oid, aid))
}