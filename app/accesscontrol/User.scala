/**
 *
 */
package accesscontrol

import play.api.db._
import play.api.Play.current
import scala.slick.driver.PostgresDriver.simple._
import scala.slick.lifted.Query
import Database.threadLocalSession
import scalaz.{ Failure, Success, Validation }
import models.{ Email, Person }
import scala.slick.lifted.MappedTypeMapper.base
import scala.slick.lifted.TypeMapper
import util.{ UserRole, StandardUser }

/**
 * Representation of a user of the website.
 * 
 * @author andreas
 * @version 0.0.6, 2015-01-11
 */
case class User(
  val username: String,
  val password: String,
  val email: Email,
  val person: Person,
  val created: Long = System.currentTimeMillis(),
  val modified: Option[Long]) {

  require(Option(username).isDefined)
  require(Option(password).isDefined && password.length() >= User.minimumPasswordLength)
  require(Option(email).isDefined)
}

object User {

  implicit lazy val db = Database.forDataSource(DB.getDataSource())
  val tablename = "User"
  val minimumPasswordLength = 8

  def insert(u: User): Validation[Throwable, User] = db withSession {
    try {
      Users.insert(u)
      Success(u)
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  def auth(username: String, password: String): Validation[Throwable, User] = db withSession {
    try {
      val result = Query(Users).where(_.username === username).where(_.password === password).firstOption
      result match {
        case None => Failure(new RuntimeException("Failed to authenticate user with username " + username))
        case _ => Success(result.get)
      }
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  def findByName(u: String): Validation[Throwable, User] = db withSession {
    try {
      val result = Query(Users).where(_.username === u).firstOption
      result match {
        case None => Failure(new RuntimeException("Failed to find user with username " + u))
        case _ => Success(result.get)
      }
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  def register(u: User): Validation[Throwable, User] = db withSession {
    try {
      require(Option(u).isDefined)
      Users.insert(u)
      Success(u)
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  /**
   * Retrieve all events from the persistence store.
   */
  def getAll(): Validation[Throwable, List[User]] = db withSession {
    try {
      Success(Query(Users).list)
    } catch {
      case e: Throwable => Failure(e)
    }
  }

}

object Users extends Table[User](User.tablename) {

  import scala.slick.lifted.MappedTypeMapper.base
  import scala.slick.lifted.TypeMapper
  
  implicit val emailMapper = base[Email, Long](e => e.id.get, id => Email.load(id).get)
  implicit val personMapper = base[Person, Long](p => p.id.get, id => Person.load(id).get)

  def username = column[String]("username", O.PrimaryKey)
  def password = column[String]("password")
  def email = column[Email]("email")
  def person = column[Person]("person")
  def created = column[Long]("created")
  def modified = column[Long]("modified", O.Nullable)
  def * = username ~ password ~ email ~ person ~ created ~ modified.? <> (User.apply _, User.unapply _)
}