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
import util.PhoneType
import util.Landline

/**
 * @author andreas
 * @version 0.0.5, 2015-01-03
 */
case class Phone(override val id: Option[Long],
  val areacode: Int,
  val extension: Int,
  val country: Country,
  val kind: PhoneType,
  override val created: Long = System.currentTimeMillis(),
  override val creator: String,
  override val modified: Option[Long] = None,
  override val modifier: Option[String]) extends Entity(id, created, creator, modified, modifier)

object Phone {

  implicit val db = Database.forDataSource(DB.getDataSource())

  val tablename = "Phone"

  def load(id: Long): Option[Phone] = db withSession {
    Query(Phones).filter(_.id === id).firstOption
  }

  private def delete(id: Long): Validation[Throwable, Boolean] = {
    try {
      val delCount = Query(Phones).filter(_.id === id).delete
      if (delCount > 0) Success(true) else Failure(new RuntimeException("Failed to delete phone number with id " + id))
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  def deleteOrgPhone(oid: Long, id: Long): Validation[Throwable, Boolean] = db withSession {
    val del = Query(OrgHasPhones).where(_.oid === oid).where(_.phid === id).delete
    if (del > 0) {
      delete(id)
    } else {
      Failure(new RuntimeException("Failed to delete the connection between the organization and the phone number. (" + oid + ", " + id + ")"))
    }
  }

  def deletePersonPhone(pid: Long, id: Long): Validation[Throwable, Boolean] = db withSession {
    try {
      // first delete association
      Query(PersonHasPhones).where(_.pid === pid).where(_.phid === id).delete
      // then delete email address
      delete(id)
    } catch {
      case t: Throwable => Failure(t)
    }
  }

  /**
   * Get the list of phone numbers an {@link Organization} has.
   *
   * Note, that the output list is sorted according to the position parameter in the relation table.
   */
  def getOrgPhones(o: Organization): Validation[Throwable, List[Phone]] = {
    require(o.id.isDefined)
    db withSession {
      try {
        val result = for {
          ohp <- OrgHasPhones.sortBy(x => (x.oid, x.pos)) if ohp.oid === o.id.get
          p <- Phones if ohp.phid === p.id
        } yield (p)
        Success(result.list)
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }
  
  /**
   * Get the {@link Phone} identified by <em>phid</em> for the {@link Person} <em>p</em>.
   */
  def getPersonPhone(p: Person, phid: Long): Validation[Throwable, Option[(Phone, UsageType, Privacy)]] = {
    require(p.id.isDefined)
    db withSession {
      try {
        val php = Query(PersonHasPhones).where(_.pid === p.id.get).where(_.phid === phid).firstOption
        val phone = load(phid)
        if (php.isDefined) {
          if (phone.isDefined) {
            Success(Some((phone.get, php.get.usage, php.get.privacy)))
          } else {
            Failure(new RuntimeException("Phone number with ID " + phid + " not found."))
          }
        } else {
          Success(None)
        }
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }
  
  /**
   * Get the {@link Phone} identified by <em>phid</em> for the {@link Organization} <em>o</em>.
   */
  def getOrgPhone(o: Organization, phid: Long): Validation[Throwable, Option[Phone]] = {
    require(o.id.isDefined)
    db withSession {
      try {
        val ohp = Query(OrgHasPhones).where(_.oid === o.id.get).where(_.phid === phid).firstOption
        val phone = load(phid)
        if (ohp.isDefined) {
          if (phone.isDefined) {
            Success(phone)
          } else {
            Failure(new RuntimeException("Phone number with ID " + phid + " not found."))
          }
        } else {
          Success(None)
        }
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }

  /**
   * Get the list of phone numbers a Person has.
   *
   * Note, that the output list is sorted according to the position parameter in the relation table.
   */
  def getPersonPhones(p: Person): Validation[Throwable, List[(Phone, UsageType, Privacy)]] = {
    require(p.id.isDefined)
    db withSession {
      try {
        val list = Query(PersonHasPhones).where(_.pid === p.id.get).list
        val result = for {
          php <- list
          ph <- Phone.load(php.phid)
        } yield ((ph, php.usage, php.privacy))
        Success(result)
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }

  /**
   * Insert the specified Phone number in the database and set it into a relation to the given {@link Organization}.
   *
   * If the person is not yet persisted it will be in a first step.
   */
  def saveOrgPhone(o: Organization, ph: Phone): Validation[Throwable, Phone] = {

    var o1 = o
    if (!o.id.isDefined) {
      o1 = Organization.saveOrUpdate(o).toOption.get
    }
    db withSession {
      try {
        val isUpdate = if (ph.id.isDefined) true else false
        val ph1 = Phone.saveOrUpdate(ph)
        if (ph1.isSuccess) {
          val l = Query(OrgHasPhones).where(_.oid === o1.id.get).list
          if (!isUpdate) {
            OrgHasPhones.insert(OrgHasPhone(o1.id.get, ph1.toOption.get.id.get, l.length + 1));
          }
          ph1
        } else Failure(ph1.toEither.left.get)
      } catch {
        case e: Throwable => Failure(e)
      }

    }
  }

  /**
   * Insert the specified Phone number in the database and set it into a relation to the given Person.
   *
   * If the person is not yet persisted it will be in a first step.
   */
  def savePersonPhone(p: Person, ph: Phone): Validation[Throwable, Phone] = {

    var p1 = p
    if (!p.id.isDefined) {
      p1 = Person.saveOrUpdate(p).toOption.get
    }
    db withSession {
      try {
        val isUpdate = if (ph.id.isDefined) true else false
        val ph1 = Phone.saveOrUpdate(ph)
        if (ph1.isSuccess) {
          val l = Query(PersonHasPhones).where(_.pid === p1.id.get).list
          if (!isUpdate) {
            PersonHasPhones.insert(PersonHasPhone(p1.id.get, ph1.toOption.get.id.get, l.length + 1));
          }
          ph1
        } else Failure(ph1.toEither.left.get)
      } catch {
        case e: Throwable => Failure(e)
      }

    }
  }

  private def saveOrUpdate(ph: Phone): Validation[Throwable, Phone] = {
    db withSession {
      if (ph.id.isDefined) {
        try {
          val count = Phones.update(ph)
          if (count == 0) {
            Failure(new RuntimeException("Failed to update phone number " + ph))
          } else {
            Success(ph)
          }
        } catch {
          case e: Throwable => Failure(e)
        }
      } else {
        try {
          val id = Phones.insert(ph)
          Success(ph.copy(id = Some(id)))
        } catch {
          case e: Throwable => Failure(e)
        }
      }
    }
  }
}

object Phones extends Table[Phone](Phone.tablename) {

  import scala.slick.lifted.MappedTypeMapper.base
  import scala.slick.lifted.TypeMapper
  import util.Personal

  implicit val countryMapper: TypeMapper[Country] = base[Country, Int](c => c.id.get, id => Country.load(id).get)
  implicit val phoneTypeMapper: TypeMapper[PhoneType] = base[PhoneType, Int](pt => pt.id, id => Landline.getPhoneType(id).get)
  implicit val usageTypeMapper: TypeMapper[UsageType] = base[UsageType, Int](ut => ut.id, id => Personal.getUsageType(id).get)
  implicit val privacyMapper: TypeMapper[Privacy] = base[Privacy, Int](p => p.id, id => MembersPrivate.getPrivacy(id).get)

  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def areacode = column[Int]("areacode")
  def extension = column[Int]("extension")
  def country = column[Country]("country")
  def kind = column[PhoneType]("kind")
  def usage = column[UsageType]("usage")
  def privacy = column[Privacy]("privacy")
  def created = column[Long]("created")
  def creator = column[String]("creator")
  def modified = column[Long]("modified", O.Nullable)
  def modifier = column[String]("modifier", O.Nullable)
  def * = id.? ~ areacode ~ extension ~ country ~ kind ~ created ~ creator ~ modified.? ~ modifier.? <> (Phone.apply _, Phone.unapply _)

  def withoutId = areacode ~ extension ~ country ~ kind ~ created ~ creator ~ modified.? ~ modifier.? returning id
  def insert = (p: Phone) => withoutId.insert(p.areacode, p.extension, p.country, p.kind, p.created, p.creator, p.modified, p.modifier)
  def update(p: Phone): Int = Phones.where(_.id === p.id).update(p.copy(modified = Some(System.currentTimeMillis())))
}

