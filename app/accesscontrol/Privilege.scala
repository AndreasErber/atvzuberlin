/**
 *
 */
package accesscontrol

import db.GenericDao
import models.Entity
import scala.slick.driver.PostgresDriver.simple._
import scala.slick.lifted.Query
import Database.threadLocalSession
import scalaz.{ Failure, Success, Validation }

/**
 * Entity to represent a privilege.
 *
 * @author andreas
 * @version 0.0.2, 2015-04-20
 */
case class Privilege(override val id: Option[Long] = None,
  name: String,
  description: Option[String] = None,
  override val created: Long = System.currentTimeMillis(),
  override val creator: String,
  override val modified: Option[Long] = None,
  override val modifier: Option[String] = None) extends Entity(id, created, creator, modified, modifier) {

  require(Option(name).isDefined)
}

object Privileges extends Table[Privilege]("Privilege") with GenericDao[Privilege] {

  override def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name")
  def description = column[String]("description", O.Nullable)
  def created = column[Long]("created")
  def creator = column[String]("creator")
  def modified = column[Long]("modified", O.Nullable)
  def modifier = column[String]("modifier", O.Nullable)

  def * = id.? ~ name ~ description.? ~ created ~ creator ~ modified.? ~ modifier.? <> (Privilege.apply _, Privilege.unapply _)

  def withoutId = name ~ description.? ~ created ~ creator ~ modified.? ~ modifier.? returning id

  def insert = db withSession { (p: Privilege) => withoutId.insert(p.name, p.description, p.created, p.creator, p.modified, p.modifier) }
  override def update(p: Privilege): Int = db withSession { Privileges.where(_.id === p.id).update(p.copy(modified = Some(System.currentTimeMillis()))) }

  def saveOrUpdate(p: Privilege): Validation[Throwable, Privilege] = db withSession {
    if (p.id.isDefined) {
      val pUpd = p.copy(modified = Some(System.currentTimeMillis()))
      try {
        val upd = this.update(pUpd)
        if (upd > 0) {
          Success(pUpd)
        } else {
          Failure(new RuntimeException("Failed to update privilege " + upd))
        }
      } catch {
        case e: Throwable => Failure(e)
      }
    } else {
      try {
        val id = this.insert(p)
        Success(p.copy(id = Some(id)))
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }
  
  /**
   * Find a privilege by its full name.
   * 
   * @param name The full name of the [[Privilege]] to return.
   */
  def findByName(name: String): Option[Privilege] = db withSession {
    if (Option(name).isDefined) {
      Query(Privileges).filter(_.name === name).firstOption
    } else None
  }
  
  /**
   * Retrieve a list of privileges as given by their IDs.
   * 
   * @param ids List of privilege identifiers.
   * @return A list of privileges that match the given identifiers.
   */
  def getSome(ids: List[Long]): Validation[Throwable, List[Privilege]] = db withSession  {
    if (ids.isEmpty) {
      return Success(Nil)
    }
    
    try {
      val query = for {
        priv <- Privileges
        if priv.id inSetBind ids
      } yield priv
      Success(query.list)
    } catch {
      case e: Throwable => Failure(e)
    }
  }
}
