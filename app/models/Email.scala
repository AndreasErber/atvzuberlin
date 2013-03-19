/**
 *
 */
package models

import play.data.validation.Constraints
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
import util.UsageType
import util.Privacy
import util.MembersPrivate

/**
 * @author andreas
 * @version 0.0.5, 2013-03-19
 */
case class Email(override val id: Option[Long],
    val address: String,
    val usage: UsageType = Personal,
    val privacy: Privacy = MembersPrivate,
    override val created: Long = System.currentTimeMillis(),
    override val creator: String,
    override val modified: Option[Long] = None,
    override val modifier: Option[String]) extends Entity(id, created, creator, modified, modifier) {

  // check the params
  require(Option(address).isDefined)
  require(Option(usage).isDefined)

  private val validator = new Constraints.EmailValidator
  require(validator.isValid(address))

  /**
   * Redefine the string representation of this class.
   *
   * @return The string representation of this instance.
   */
  override def toString(): String = {

    var result = ""
    if (id.isDefined) {
      result = "(" + id.get + ") "
    }
    result + address + " (" + this.usage.toString() + ")"
  }

  /**
   * Redefine the inherited method to limit equality to instances of the same type.
   *
   * @param other The instance to compare this instance with.
   * @return <code>true</code> if the other instance is of the same type, <code>false</code> otherwise.
   */
  override def canEqual(other: Any) = other.isInstanceOf[Email]

  /**
   * Redefine the comparison function.
   *
   * @param other The instance to compare to this instance.
   * @return <code>true</code> if the instance are the same or equal, <code>false</code> otherwise.
   */
  override def equals(other: Any): Boolean = other match {
    case that: Email =>
      if (this eq that) true
      else {
        (that.## == this.##) &&
          (that canEqual this) &&
          (this.address == that.address) &&
          (this.usage == that.usage)
      }
    case _ => false
  }

  /**
   * Redefine the hashing function.
   *
   * @return The hash of this object.
   */
  override def hashCode(): Int = {

    var hash = 17
    hash = 31 * hash + address.hashCode
    hash = 31 * hash + usage.hashCode
    hash
  }
}

object Email {

  implicit val db = Database.forDataSource(DB.getDataSource())
  val tablename = "Email"

  def load(id: Long): Option[Email] = db withSession {
    Query(Emails).filter(_.id === id).firstOption
  }

  private def delete(id: Long): Validation[Throwable, Boolean] = {
    try {
      val delCount = Query(Emails).filter(_.id === id).delete
      if (delCount > 0) Success(true) else Failure(new RuntimeException("Failed to delete email address with id " + id))
    } catch {
      case e: Throwable => Failure(e)
    }
  }
  
  def deleteOrgEmail(oid: Long, id: Long): Validation[Throwable, Boolean] = db withSession {
    val del = Query(OrgHasEmail).where(_.oid === oid).where(_.eid === id).delete
    if (del > 0) {
      delete(id)
    } else {
      Failure(new RuntimeException("Failed to delete the connection between the organization and the email address. (" + oid + ", " + id + ")"))
    }
  }

  def deletePersonEmail(pid: Long, id: Long): Validation[Throwable, Boolean] = db withSession {
    val del = Query(PersonHasEmail).where(_.pid === pid).where(_.eid === id).delete
    if (del > 0) {
      delete(id)
    } else {
      Failure(new RuntimeException("Failed to delete the connection between the person and the email address. (" + pid + ", " + id + ")"))
    }
  }

  def findByAddress(e: String): Validation[Throwable, Email] = {
    db withSession {
      try {
        val result = Query(Emails).where(_.address === e).firstOption
        result match {
          case None => Failure(new RuntimeException("Failed to find email " + e))
          case _    => Success(result.get)
        }
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
  def getPersonEmails(p: Person): Validation[Throwable, List[Email]] = {
    require(p.id.isDefined)
    db withSession {
      try {
        val result = for {
          phe <- PersonHasEmail.sortBy(x => (x.pid, x.pos)) if phe.pid === p.id.get
          e <- Emails if phe.eid === e.id
        } yield (e)
        Success(result.list)
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }

  /**
   * Insert the specified Email item in the database and set it into a relation to the given Person.
   *
   * If the person is not yet persisted it will be in a first step.
   */
  def savePersonEmail(p: Person, e: Email): Validation[Throwable, Email] = {

    var p1 = p
    if (!p.id.isDefined) {
      p1 = Person.saveOrUpdate(p).toOption.get
    }
    db withSession {
      try {
        val isUpdate = if (e.id.isDefined) true else false
        val e1 = Email.saveOrUpdate(e)
        if (e1.isSuccess) {
          if (!isUpdate) {
            val l = Query(PersonHasEmail).where(_.pid === p1.id.get).list
            PersonHasEmail.insert((p1.id.get, e1.toOption.get.id.get, l.length + 1));
          }
          e1
        } else Failure(e1.fail.toOption.get)
      } catch {
        case e: Throwable => Failure(e)
      }

    }
  }

  private def saveOrUpdate(e: Email): Validation[Throwable, Email] = {
    db withSession {
      if (e.id.isDefined) {
        try {
          val count = Emails.update(e)
          if (count == 0) {
            Failure(new RuntimeException("Failed to update email address " + e))
          } else {
            Success(e)
          }
        } catch {
          case e: Throwable => Failure(e)
        }
      } else {
        try {
          val id = Emails.insert(e)
          Success(e.copy(id = Some(id)))
        } catch {
          case e: Throwable => Failure(e)
        }
      }
    }
  }
}

object Emails extends Table[Email](Email.tablename) {

  import scala.slick.lifted.MappedTypeMapper.base
  import scala.slick.lifted.TypeMapper
  import util.Personal
  implicit val usageTypeMapper: TypeMapper[UsageType] = base[UsageType, Int](ut => ut.id, id => Personal.getUsageType(id).get)
  implicit val privacyMapper: TypeMapper[Privacy] = base[Privacy, Int](p => p.id, id => MembersPrivate.getPrivacy(id).get)

  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def address = column[String]("address")
  def usage = column[UsageType]("usage")
  def privacy = column[Privacy]("privacy")
  def created = column[Long]("created")
  def creator = column[String]("creator")
  def modified = column[Long]("modified", O.Nullable)
  def modifier = column[String]("modifier", O.Nullable)
  def * = id.? ~ address ~ usage ~ privacy ~ created ~ creator ~ modified.? ~ modifier.? <> (Email.apply _, Email.unapply _)

  def withoutId = address ~ usage ~ privacy ~ created ~ creator ~ modified.? ~ modifier.? returning id
  def insert = (e: Email) => withoutId.insert(e.address, e.usage, e.privacy, e.created, e.creator, e.modified, e.modifier)
  def update(e: Email): Int = Emails.where(_.id === e.id).update(e.copy(modified = Some(System.currentTimeMillis())))
}

object PersonHasEmail extends Table[(Long, Long, Int)]("PersonHasEmail") {
  def pid = column[Long]("pid")
  def eid = column[Long]("eid")
  def pos = column[Int]("position")
  def * = pid ~ eid ~ pos
  def person = foreignKey("person_fk", pid, Persons)(_.id)
  def email = foreignKey("email_fk", eid, Emails)(_.id)
  def pk = primaryKey("pk_personhasemail", (pid, eid))
}

object OrgHasEmail extends Table[(Long, Long, Int)]("OrgHasEmail") {
  def oid = column[Long]("oid")
  def eid = column[Long]("eid")
  def pos = column[Int]("position")
  def * = oid ~ eid ~ pos
  def organization = foreignKey("org_fk", oid, Organizations)(_.id)
  def email = foreignKey("email_fk", eid, Emails)(_.id)
  def pk = primaryKey("pk_orghasemail", (oid, eid))
}