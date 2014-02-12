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
import util.UserRole
import util.StandardUser
import scalaz.Validation
import scalaz.Failure
import scalaz.Success

/**
 * @author andreas
 * @version 0.0.3, 2013-03-04
 */
case class User(
  val username: String,
  val password: String,
  val email: Email,
  val person: Long,
  val role: UserRole = StandardUser,
  val created: Long = System.currentTimeMillis(),
  val modified: Option[Long]) {

  require(Option(username).isDefined)
  require(Option(password).isDefined && password.length() > 8)
  require(Option(email).isDefined)
}

object User {

  implicit lazy val db = Database.forDataSource(DB.getDataSource())
  val tablename = "User"

  def insert(u: User): Validation[Throwable, User] = {
    db withSession {
      try {
        Users.insert(u)
        Success(u)
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }

  def auth(username: String, password: String): Validation[Throwable, User] = {
    db withSession {
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
  }

  def findByName(u: String): Validation[Throwable, User] = {
    db withSession {
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
  }

  def register(u: User): Validation[Throwable, User] = {
    db withSession {
      try {
        require(Option(u).isDefined)
        Users.insert(u)
        Success(u)
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }
}

object Users extends Table[User](User.tablename) {

  import scala.slick.lifted.MappedTypeMapper.base
  import scala.slick.lifted.TypeMapper
  implicit val emailMapper: TypeMapper[Email] = base[Email, Long](e => e.id.get, id => Email.load(id).get)
  implicit val roleMapper: TypeMapper[UserRole] = base[UserRole, Int](ur => ur.id, id => StandardUser.getUserRole(id).get)

  def username = column[String]("username", O.PrimaryKey)
  def password = column[String]("password")
  def email = column[Email]("email")
  def person = column[Long]("person")
  def role = column[UserRole]("role", O.Default(StandardUser))
  def created = column[Long]("created")
  def modified = column[Long]("modified", O.Nullable)
  def * = username ~ password ~ email ~ person ~ role ~ created ~ modified.? <> (User.apply _, User.unapply _)
}