/**
 *
 */
package models

import java.text.SimpleDateFormat
import java.util.Calendar
import java.sql.Date
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
 * @version 0.0.7, 2013-04-12
 */
case class Person(
    override val id: Option[Long] = None,
    val lastname: String,
    val firstname: Option[String] = None,
    val nickname: Option[String] = None,
    val birth: Option[Date] = None,
    val death: Option[Date] = None,
    val gender: Char = 'm',
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
    require(System.currentTimeMillis() > birth.get.getTime())

  def name: String = {
    val name: StringBuffer = new StringBuffer
    if (this.firstname.isDefined) {
      name.append(this.firstname.get + " ")
    } 
    name.append(this.lastname)  
    name.toString()
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
    name.toString()
  }
  /**
   * Retrieve the current age of the person.
   */
  def age(): Int = {

    val year = new SimpleDateFormat("yyyy")
    val month = new SimpleDateFormat("MM")
    val day = new SimpleDateFormat("dd");

    val cal = Calendar.getInstance()
    var age = cal.get(Calendar.YEAR) - year.format(this.birth.get).toInt
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
  override def toString(): String = {

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
  def getAll(): Validation[Throwable, List[Person]] = db withSession {
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

  /**
   * Count the number of occurrences of persons.
   */
  def count(): Int = db withSession {
    Persons.count
  }
}

object Persons extends Table[Person](Person.tablename) {

  import scala.slick.lifted.MappedTypeMapper.base
  import scala.slick.lifted.TypeMapper

  implicit val CharMapper: TypeMapper[Char] = base[Char, String](d => d.toString(), t => t(0))

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

  def withoutId = lastname ~ firstname.? ~ nickname.? ~ birth.? ~ death.?  ~ gender ~ created ~ creator ~ modified.? ~ modifier.? returning id
  def insert = (p: Person) => withoutId.insert(p.lastname, p.firstname, p.nickname, p.birth, p.death, p.gender, p.created, p.creator, p.modified, p.modifier)
  def update(p: Person): Int = Persons.where(_.id === p.id).update(p.copy(modified = Some(System.currentTimeMillis())))
  def count(): Int = Persons.count
}


