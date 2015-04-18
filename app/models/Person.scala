/**
 *
 */
package models

import java.text.SimpleDateFormat
import java.util.Calendar
import java.sql.Date
import play.api.db._
import play.api.Play.current
import scala.slick.driver.PostgresDriver.simple._
import Database.threadLocalSession
import scala.slick.lifted.Query
import scalaz.{Failure, Success, Validation}
import util.MemberState

/**
 * Entity to describe a person.
 * 
 * @author andreas
 * @version 0.0.8, 2015-04-18
 */
case class Person(
  override val id: Option[Long] = None,
  lastname: String,
  firstname: Option[String] = None,
  nickname: Option[String] = None,
  birth: Option[Date] = None,
  death: Option[Date] = None,
  gender: Char = 'm',
  override val created: Long = System.currentTimeMillis(),
  override val creator: String,
  override val modified: Option[Long] = None,
  override val modifier: Option[String] = None) extends Entity(id, created, creator, modified, modifier) {

  // lastname is the only required argument
  require(Option(lastname).isDefined)

  // only certain characters make a useful gender identification
  require(gender == 'm' || gender == 'f' || gender == 'M' || gender == 'F')

  // if birth day is defined it should be in the past
  if (birth.isDefined)
    require(System.currentTimeMillis() > birth.get.getTime)

  def name: String = {
    val name: StringBuffer = new StringBuffer
    if (this.firstname.isDefined) {
      name.append(this.firstname.get + " ")
    }
    name.append(this.lastname)
    name.toString
  }

  def fullname: String = {
    val name: StringBuffer = new StringBuffer
    if (this.firstname.isDefined) {
      name.append(this.firstname.get + " ")
    }
    name.append(this.lastname)
    if (this.nickname.isDefined) {
      name.append(" - " + this.nickname.get)
    }
    name.toString
  }
  
  /**
   * Retrieve the current age of the person.
   */
  def age(): Int = {

    val year = new SimpleDateFormat("yyyy")
    val month = new SimpleDateFormat("MM")
    val day = new SimpleDateFormat("dd")

    val cal = Calendar.getInstance()
    val age = cal.get(Calendar.YEAR) - year.format(this.birth.get).toInt
    val currM = cal.get(Calendar.MONTH) + 1
    val birthM = month.format(this.birth.get).toInt
    if (currM < birthM) {

      return age - 1
    } else if (currM == birthM) {

      if (cal.get(Calendar.DATE) < day.format(this.birth.get).toInt) {

        return age - 1
      }
    }
    age
  }

  /**
   * Provide a useful string representation of a person.
   */
  override def toString: String = {

    var out = ""
    if (this.id.isDefined) {
      out += "(" + this.id.get + ") "
    }

    out += this.lastname

    if (this.firstname.isDefined) {
      out += ", " + this.firstname.get
    }

    if (this.birth.isDefined) {
      out += ", " + Person.SDF.format(this.birth.get)
    }

    out + ", " + this.gender
  }

  /**
   * Redefine the inherited method to limit equality to instances of the same type.
   *
   * @param other The instance to compare this instance with.
   * @return <code>true</code> if the other instance is of the same type, <code>false</code> otherwise.
   */
  override def canEqual(other: Any) = other.isInstanceOf[Person]

  /**
   * Redefine the comparison function.
   *
   * @param other The instance to compare to this instance.
   * @return <code>true</code> if the instance are the same or equal, <code>false</code> otherwise.
   */
  override def equals(other: Any): Boolean = other match {
    case that: Person =>
      if (this eq that) true
      else {
        (that.## == this.##) &&
          (that canEqual this) &&
          (this.lastname == that.lastname) &&
          (this.firstname == that.firstname) &&
          (this.birth == that.birth) &&
          (this.gender == that.gender)
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
    hash = 31 * hash + this.lastname.hashCode()
    if (this.firstname.isDefined)
      hash = 31 * hash + this.firstname.hashCode()
    hash = 31 * hash + this.birth.hashCode
    hash = 31 * hash + this.gender
    hash
  }

}

/**
 * Partner object holding the database access members.
 */
object Person {

  implicit lazy val db = Database.forDataSource(DB.getDataSource())
  val tablename = "Person"

  // date format for the birth date
  val SDF = new SimpleDateFormat("yyyy-MM-dd")

  /**
   * Retrieve all persons from the persistence store.
   */
  def getAll: Validation[Throwable, List[Person]] = db withSession {
    try {
      Success(Query(Persons).list)
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  /**
   * Load the person related to the given identifier.
   */
  def load(id: Long): Option[Person] = db withSession {
    Query(Persons).filter(_.id === id).firstOption
  }

  /**
   * Persist a new person instance or update an existing one.
   */
  def saveOrUpdate(p: Person): Validation[Throwable, Person] = {
    db withSession {

      require(Option(p).isDefined)
      // if the object has an identifier it is an update
      if (p.id.isDefined) {
        try {
          val count = Persons.update(p)
          if (count == 0) {
            Failure(new RuntimeException("Failed to update person " + p))
          } else {
            Success(p)
          }
        } catch {
          case e: Throwable => Failure(e)
        }
      } // objects without identifier are new and must be inserted
      else {
        try {
          val id = Persons.insert(p)
          Success(p.copy(id = Some(id)))
        } catch {
          case e: Throwable => Failure(e)
        }
      }
    }
  }

  /**
   * Delete the person identified by id.
   */
  def delete(id: Long): Validation[Throwable, Boolean] = db withSession {
    try {
      val delCount = Query(Persons).filter(_.id === id).delete
      if (delCount > 0) Success(true) else Failure(new RuntimeException("Failed to delete person with id " + id))
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  def deleteCascading(id: Long): Validation[Throwable, Boolean] = {
    try {
      Success(this.deleteCompletely(id))
    } catch {
      case t: Throwable => Failure(t)
    }
  }

  /**
   * Deletes not only the [[Person]] instance but also its associations to other entities.
   *
   * The method will remove
   * <ul>
   *   <li>entries in [[PersonHasTitles]] having <em>id</em> for the pid</li>
   *   <li>entries in [[PersonHasEmails]] having <em>id</em> for the pid and the entries in [[Email]]s
   *       referenced by the deleted entries of [[PersonHasEmails]] entries</li>
   *   <li>entries in [[PersonHasPhones]] having <em>id</em> for the pid and the entries in [[Phones]]
   *       referenced by the deleted entries of [[PersonHasPhones]] entries</li>
   *   <li>entries in [[PersonHasHomepages]] having <em>id</em> for the pid and the entries in [[Homepages]]
   *       referenced by the deleted entries of [[PersonHasHomepages]] entries</li>
   *   <li>the corresponding entry in [[PersonAdditionalInfo]]</li>
   *   <li>the entry in [[Persons]] referenced by <em>id</em></li>
   * </ul>
   *
   * @param id The identifier of the [[Person]] and its associations to delete.
   * @return <code>true</code> if all associations and the [[Person]] identified by <em>id</em> could be removed
   *          successfully.
   */
  private def deleteCompletely(id: Long): Boolean = db withTransaction {
    // delete association with titles
    Query(PersonHasTitles).where(_.pid === id).delete

    // remove associations to emails and the emails themselves
    val pheList = Query(PersonHasEmails).where(_.pid === id).list
    Query(PersonHasEmails).where(_.pid === id).delete
    val emailQuery = for {
      email <- Emails
      if email.id inSetBind pheList.map(phe => phe.eid)
    } yield email
    emailQuery.delete

    // remove associations to phone numbers and the phone numbers themselves
    val phpList = Query(PersonHasPhones).where(_.pid === id).list
    Query(PersonHasPhones).where(_.pid === id).delete
    val phoneQuery = for {
      phone <- Phones
      if phone.id inSetBind phpList.map(php => php.phid)
    } yield phone
    phoneQuery.delete

    // delete associations with homepages and the homepage entries as well
    val phhList = Query(PersonHasHomepages).where(_.pid === id).list
    Query(PersonHasHomepages).where(_.pid === id).delete
    val homepageQuery = for {
      homepage <- Homepages
      if homepage.id inSetBind phhList.map(phh => phh.hid)
    } yield homepage

    Query(PersonAdditionalInfos).where(_.id === id).delete
    Query(Persons).where(_.id === id).delete
    true
  }

  /**
   * Retrieve all [[Person]]s having the given [[MemberState]].
   *
   * Note, the information about the [[MemberState]] of a [[Person]] is kept in the accompanying
   * [[PersonAdditionalInfos]] instance. This method only wraps the identically named method in 
   * [[PersonAdditionalInfos]].
   *
   * @param status The [[MemberState]] to filter by.
   * @return A [[Validation]] with a [[List]] of [[Person]]s having the given status or the
   *          [[Throwable]] in case of error.
   */
  def getAllByStatus(status: MemberState): Validation[Throwable, List[Person]] = db withSession {
    PersonAdditionalInfos.getAllByStatus(status)
  }

  /**
   * Retrieve all [[Person]]s having the given [[MemberState]]s.
   *
   * Note, the information about the [[MemberState]] of a [[Person]] is kept in the accompanying
   * [[PersonAdditionalInfo]] instance.
   *
   * @param status The [[MemberState]]s to filter by.
   * @return A [[Validation]] with a [[List]] of [[Person]]s having the given status or the
   *          [[Throwable]] in case of error.
   */
  def getAllByStatus(status: List[MemberState]): Validation[Throwable, List[Person]] = db withSession {
    PersonAdditionalInfos.getAllByStatus(status)
  }

  /**
   * Retrieve the [[Person]]s that match the identifiers in the given <em>ids</em> list.
   *
   * Note, this method needs to be called within an existing database session context.
   *
   * @param ids Identifiers of [[Person]]s
   * @return A [[Validation]] with a [[List]] of [[Person]]s having one of the given IDs or the
   *          [[Throwable]] in case of error.
   */
  private def getByIds(ids: List[Long]): Validation[Throwable, List[Person]] = {
    try {
      val q = for (
        p <- Persons if p.id inSetBind ids
      ) yield p
      Success(q.list)
    } catch {
      case t: Throwable => Failure(t)
    }
  }

  /**
   * Count the number of occurrences of persons.
   */
  def count(): Int = db withSession {
    Persons.count()
  }

  def getByNickname(nick: String): Validation[Throwable, Person] = db withSession {
    try {
      Success(Query(Persons).filter(_.nickname === nick).firstOption.get)
    } catch {
      case e: Throwable => Failure(e)
    }
  }
}

object Persons extends Table[Person](Person.tablename) {

  import scala.slick.lifted.MappedTypeMapper.base
  import scala.slick.lifted.TypeMapper

  implicit val CharMapper: TypeMapper[Char] = base[Char, String](d => d.toString, t => t(0))

  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def lastname = column[String]("lastname")
  def firstname = column[String]("firstname", O.Nullable)
  def nickname = column[String]("nickname", O.Nullable)
  def birth = column[Date]("birth", O.Nullable)
  def death = column[Date]("death", O.Nullable)
  def gender = column[Char]("gender", O.Default('m'), O.DBType("CHAR(1)"))
  def created = column[Long]("created")
  def creator = column[String]("creator")
  def modified = column[Long]("modified", O.Nullable)
  def modifier = column[String]("modifier", O.Nullable)
  def * = id.? ~ lastname ~ firstname.? ~ nickname.? ~ birth.? ~ death.? ~ gender ~ created ~ creator ~ modified.? ~ modifier.? <> (Person.apply _, Person.unapply _)

  def withoutId = lastname ~ firstname.? ~ nickname.? ~ birth.? ~ death.? ~ gender ~ created ~ creator ~ modified.? ~ modifier.? returning id
  def insert = (p: Person) => withoutId.insert(p.lastname, p.firstname, p.nickname, p.birth, p.death, p.gender, p.created, p.creator, p.modified, p.modifier)
  def update(p: Person): Int = Persons.where(_.id === p.id).update(p.copy(modified = Some(System.currentTimeMillis())))
  def count(): Int = Persons.count()
}


