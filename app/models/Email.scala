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
import util.{Privacy, UsageType}

/**
 * Representation of an [[Email]] address.
 * 
 * @author andreas
 * @version 0.0.10, 2015-04-18
 */
case class Email(override val id: Option[Long],
    address: String,
    override val created: Long = System.currentTimeMillis(),
    override val creator: String,
    override val modified: Option[Long] = None,
    override val modifier: Option[String]) extends Entity(id, created, creator, modified, modifier) {

  // check the params
  require(Option(address).isDefined)

  // TODO: FIXME
//  private val validator = new Constraints.EmailValidator
//  require(validator.isValid(address))

  /**
   * Redefine the string representation of this class.
   *
   * @return The string representation of this instance.
   */
  override def toString: String = {

    var result = ""
    if (id.isDefined) {
      result = "(" + id.get + ") "
    }
    result + address
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
          (this.address == that.address)
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
    hash
  }
}

object Email {

  implicit val db = Database.forDataSource(DB.getDataSource())
  val tablename = "Email"

  def load(id: Long): Validation[Throwable, Option[Email]] = db withSession {
    try {
      Success(Query(Emails).filter(_.id === id).firstOption)
    } catch {
      case t: Throwable => Failure(t)
    }
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
    val del = Query(OrgHasEmails).where(_.oid === oid).where(_.eid === id).delete
    if (del > 0) {
      delete(id)
    } else {
      Failure(new RuntimeException("Failed to delete the connection between the organization and the email address. (" + oid + ", " + id + ")"))
    }
  }

  def deletePersonEmail(pid: Long, id: Long): Validation[Throwable, Boolean] = db withSession {
    val del = Query(PersonHasEmails).where(_.pid === pid).where(_.eid === id).delete
    if (del > 0) {
      delete(id)
    } else {
      Failure(new RuntimeException("Failed to delete the connection between the person and the email address. (" + pid + ", " + id + ")"))
    }
  }

  def findByAddress(e: String): Validation[Throwable, Option[Email]] = {
    db withSession {
      try {
        val result = Query(Emails).where(_.address === e).firstOption
        result match {
          case None => Failure(new RuntimeException("Failed to find email " + e))
          case _    => Success(result)
        }
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }

  /**
   * Get the [[Email]] identified by <em>eid</em> for the [[Organization]] <em>o</em>.
   */
  def getOrgEmail(o: Organization, eid: Long): Validation[Throwable, Option[(Email, OrgHasEmail)]] = {
    require(o.id.isDefined)
    db withSession {
      try {
        val ohe = Query(OrgHasEmails).where(_.oid === o.id.get).where(_.eid === eid).firstOption
        val emailV = load(eid)
        if (emailV.isSuccess) {
          if (ohe.isDefined) {
            val emailOp = emailV.toOption.get
            if (emailOp.isDefined) {
              Success(Some(emailOp.get, ohe.get))
            } else {
              Failure(new RuntimeException("Email address with ID " + eid + " not found."))
            }
          } else {
            Success(None)
          }
        } else {
          throw emailV.toEither.left.get
        }
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }

  /**
   * Get the list of [[Email]] addresses an [[Organization]] has.
   *
   * Note, that the output list is sorted according to the position parameter in the relation table.
   */
  def getOrgEmails(o: Organization): Validation[Throwable, List[(Email, OrgHasEmail)]] = {
    require(o.id.isDefined)
    db withSession {
      try {
        val result = for {
          ohe <- OrgHasEmails.sortBy(x => (x.oid, x.pos)) if ohe.oid === o.id.get
          e <- Emails if ohe.eid === e.id
        } yield (e, ohe)
        Success(result.list)
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }

  /**
   * Get the [[Email]] identified by <em>eid</em> for the [[Person]] <em>p</em>.
   */
  def getPersonEmail(p: Person, eid: Long): Validation[Throwable, Option[(Email, PersonHasEmail)]] = {
    require(p.id.isDefined)
    db withSession {
      try {
        val phe = Query(PersonHasEmails).where(_.pid === p.id.get).where(_.eid === eid).firstOption
        val emailV = load(eid)
        if (emailV.isSuccess) {
          if (phe.isDefined) {
            val emailOp = emailV.toOption.get
            if (emailOp.isDefined) {
              Success(Some((emailOp.get, phe.get)))
            } else {
              Failure(new RuntimeException("Email address with ID " + eid + " not found."))
            }
          } else {
            Success(None)
          }
        } else {
          throw emailV.toEither.left.get
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
  def getPersonEmails(p: Person): Validation[Throwable, List[(Email, PersonHasEmail)]] = db withSession {
    require(p.id.isDefined)
    db withSession {
      try {
        val result: Query[(Emails.type, PersonHasEmails.type), (Email, PersonHasEmail)] = for {
          phe <- PersonHasEmails.sortBy(x => (x.pid, x.pos)) if phe.pid === p.id.get
          e <- Emails if phe.eid === e.id
        } yield (e, phe)
        Success(result.list)
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }

  /**
   * Insert the specified [[Email]] item in the database and set it into a relation to the given [[Person]].
   *
   * If the person is not yet persisted it will be in a first step.
   */
  def savePersonEmail(p: Person, e: Email, u: UsageType, pr: Privacy, pos: Int = 0): Validation[Throwable, (Email, PersonHasEmail)] = {

    var p1 = p
    if (!p.id.isDefined) {
      p1 = Person.saveOrUpdate(p).toOption.get
    }
    db withSession {
      try {
        val isUpdate = if (e.id.isDefined) true else false
        val e1 = Email.saveOrUpdate(e)
        if (e1.isSuccess) {
          var position = -1
          if (!isUpdate) {
            val l = Query(PersonHasEmails).where(_.pid === p1.id.get).list
            position = l.length +1
            PersonHasEmails.insert(PersonHasEmail(p1.id.get, e1.toOption.get.id.get, position))
          } else {
            val oldPos = Query(PersonHasEmails).where(_.pid === p1.id.get).where(_.eid === e1.toOption.get.id.get).firstOption.get.pos
            position = if (pos == 0) oldPos else pos
            PersonHasEmails.update(PersonHasEmail(p1.id.get, e1.toOption.get.id.get, position, u, pr))
            if (oldPos > position) {
              val l = Query(PersonHasEmails).where(_.pid === p1.id.get).where(_.pos >= position).list.sortBy(x => x.pos)
              for (phe1 <- l) {
                if (phe1.eid != e1.toOption.get.id.get && phe1.pos > oldPos && phe1.pos <= position) {
                  PersonHasEmails.update(phe1.copy(pos = phe1.pos + 1))
                }
              }
            } else if (oldPos < position) {
              val l = Query(PersonHasEmails).where(_.pid === p1.id.get).where(_.pos <= position).list.sortBy(x => x.pos)
              for (phe1 <- l) {
                if (phe1.eid != e1.toOption.get.id.get && phe1.pos < oldPos && phe1.pos >= position) {
                  PersonHasEmails.update(phe1.copy(pos = phe1.pos - 1))
                }
              }
            }
          }
          Success((e1.toOption.get, PersonHasEmail(p.id.get, e1.toOption.get.id.get, position, u, pr)))
        } else Failure(e1.toEither.left.get)
      } catch {
        case e: Throwable => Failure(e)
      }

    }
  }

  /**
   * Insert the specified [[Email]] item in the database and set it into a relation to the given [[Organization]].
   *
   * If the person is not yet persisted it will be in a first step.
   */
  def saveOrgEmail(o: Organization, e: Email, pos: Int = 0): Validation[Throwable, (Email, OrgHasEmail)] = {

    var o1 = o
    if (!o.id.isDefined) {
      o1 = Organization.saveOrUpdate(o).toOption.get
    }
    db withSession {
      try {
        val isUpdate = if (e.id.isDefined) true else false
        val e1 = Email.saveOrUpdate(e)
        if (e1.isSuccess) {
          var position = -1
          if (!isUpdate) {
            val l = Query(OrgHasEmails).where(_.oid === o1.id.get).list
            position = l.length + 1
            OrgHasEmails.insert(OrgHasEmail(o1.id.get, e1.toOption.get.id.get, position))
          } else {
            val oldPos = Query(OrgHasEmails).where(_.oid === o1.id.get).where(_.eid === e1.toOption.get.id.get).firstOption.get.pos
            position = if (pos == 0) oldPos else pos
            OrgHasEmails.update(OrgHasEmail(o.id.get, e1.toOption.get.id.get, position))
            if (oldPos > position) {
              val l = Query(OrgHasEmails).where(_.oid === o1.id.get).where(_.pos >= position).list.sortBy(x => x.pos)
              for (ohe1 <- l) {
                if (ohe1.eid != e1.toOption.get.id.get && ohe1.pos > oldPos && ohe1.pos <= e1.toOption.get.id.get) {
                  OrgHasEmails.update(ohe1.copy(pos = ohe1.pos + 1))
                }
              }
            } else if (oldPos < position) {
              val l = Query(OrgHasEmails).where(_.oid === o1.id.get).where(_.pos <= position).list.sortBy(x => x.pos)
              for (ohe1 <- l) {
                if (ohe1.eid != e1.toOption.get.id.get && ohe1.pos < oldPos && ohe1.pos >= e1.toOption.get.id.get) {
                  OrgHasEmails.update(ohe1.copy(pos = ohe1.pos - 1))
                }
              }
            }
          }
          Success((e1.toOption.get, OrgHasEmail(o.id.get,e1.toOption.get.id.get, position)))
        } else Failure(e1.toEither.left.get)
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

/**
 * @author andreas
 * @version 0.0.3, 2013-04-12
 */
object Emails extends Table[Email](Email.tablename) {

  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def address = column[String]("address")
  def created = column[Long]("created")
  def creator = column[String]("creator")
  def modified = column[Long]("modified", O.Nullable)
  def modifier = column[String]("modifier", O.Nullable)
  def * = id.? ~ address ~ created ~ creator ~ modified.? ~ modifier.? <> (Email.apply _, Email.unapply _)

  def withoutId = address ~ created ~ creator ~ modified.? ~ modifier.? returning id
  def insert = (e: Email) => withoutId.insert(e.address, e.created, e.creator, e.modified, e.modifier)
  def update(e: Email): Int = Emails.where(_.id === e.id).update(e.copy(modified = Some(System.currentTimeMillis())))
}
