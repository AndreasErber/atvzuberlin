/**
 *
 */
package models

import play.api.db._
import play.api.Play.current
import scala.slick.driver.PostgresDriver.simple._
import Database.threadLocalSession
import scala.slick.lifted.Query
import scalaz.Failure
import scalaz.Success
import scalaz.Validation
import db.GenericDao

/**
 * Entity to temporarily keep a registration request for a given username with a generated link where the user can register.
 * 
 * @author andreas
 * @version 0.0.3, 2015-04-18
 */
case class Registration (
  override val id: Option[Long] = None,
  username: String,
  link: String,
  email: Email,
  expires: Long,
  override val created: Long = System.currentTimeMillis(),
  override val creator: String,
  override val modified: Option[Long] = None,
  override val modifier: Option[String]) extends Entity(id, created, creator, modified, modifier)

/**
 * Partner object to provide static functionality.
 * 
 * @author andreas
 * @version 0.0.2, 2015-04-18
 */
object Registration {
  
  implicit val db = Database.forDataSource(DB.getDataSource())
  val tablename = "Registration"
    
  def insert(r: Registration): Long = db withSession {
    Registrations.insert(r)
  }

  /**
   * Retrieve a registration entry by the given <em>link</em>.
   * 
   * Finding no match is interpreted as an error and will return a [[NoSuchElementException]] within a
   * [[Failure]].
   * 
   * @param link The link to identify the registration with.
   * @return A [[Validation]] holding the registration that was matched or the [[Throwable]] in case of error.
   */
  def getByLink(link: String): Validation[Throwable, Registration] = db withSession{
    try {
      val registrationOp = Query(Registrations).where(_.link === link).firstOption
      if (registrationOp.isDefined) {
      Success(registrationOp.get)
      } else {
        Failure(new NoSuchElementException())
      }
    } catch {
      case t: Throwable => Failure(t)
    }
  }
  
  /**
   * Retrieve a registration entry by the given <em>username</em>.
   * 
   * Finding no match is interpreted as an error and will return a [[NoSuchElementException]] within a
   * [[Failure]].
   * 
   * @param username The username to identify the registration with.
   * @return A [[Validation]] holding the registration that was matched or the [[Throwable]] in case of error.
   */
  def getByUsername(username: String):Validation[Throwable, Registration] = db withSession{
    try {
      val registrationOp = Query(Registrations).where(_.username === username).firstOption
      if (registrationOp.isDefined) {
      Success(registrationOp.get)
      } else {
        Failure(new NoSuchElementException())
      }
    } catch {
      case t: Throwable => Failure(t)
    }
  } 
  
  /**
   * Delete the registration with the given <em>id</em>.
   * 
   * @param id The identifier of the [[Registration]] to delete.
   * @return A [[Validation]] holding the number of items that were deleted or the [[Throwable]] in case of
   *          error.
   */
  def delete(id: Long): Validation[Throwable, Int] = db withSession {
    try {
      Success(Registrations.delete(id))
    } catch {
      case t: Throwable => Failure(t)
    }
  }
}

/**
 * Database functionality for [[Registration]].
 */
object Registrations extends Table[Registration](Registration.tablename)  with GenericDao[Registration] {
  
  import scala.slick.lifted.MappedTypeMapper.base
  implicit val emailMapper = base[Email, Long](e => e.id.get, id => Email.load(id).toOption.get.get)
  
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def username = column[String]("username")
  def link = column[String]("link")
  def email = column[Email]("email")
  def expires = column[Long]("expires")
  def created = column[Long]("created")
  def creator = column[String]("creator")
  def modified = column[Long]("modified", O.Nullable)
  def modifier = column[String]("modifier", O.Nullable)
  
  def * = id.? ~ username ~ link ~ email ~ expires ~ created ~ creator ~ modified.? ~ modifier.? <> (Registration.apply _, Registration.unapply _)
  
  def withoutId = username ~ link ~ email ~ expires ~ created ~ creator ~ modified.? ~ modifier.? returning id
  
  def insert = (reg: Registration) => withoutId.insert(reg.username, reg.link, reg.email, reg.expires, reg.created, reg.creator, reg.modified, reg.modifier)
  
  override def update(reg: Registration) : Int = Registrations.where(_.id === reg.id).update(reg.copy(modified = Some(System.currentTimeMillis())))
}