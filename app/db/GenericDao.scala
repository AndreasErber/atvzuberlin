/**
 *
 */
package db

import models.Entity
import play.api.db._
import play.api.Play.current
import scala.slick.driver.PostgresDriver.simple._
import scala.slick.lifted.Query
import Database.threadLocalSession
import scalaz.Validation
import scalaz.Failure
import scalaz.Success

/**
 * @author aer
 * @version 0.0.1, 2013-07-27
 */
trait GenericDao[T <: Entity] {

  self: Table[T] =>

  implicit val db = Database.forDataSource(DB.getDataSource())

  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

  def * : scala.slick.lifted.ColumnBase[T]

  def autoInc = * returning id

  def get(id: Long): Option[T] = {
    Query(this).filter(_.id === id).firstOption
  }

  def getAll(): Validation[Throwable, List[T]] =
    db withSession {
      def q = Query(this).sortBy(t => t.id).list
      try {
        Success(q)
      } catch {
        case e: Throwable => Failure(e)
      }
    }

  def insert(entity: T) = {
    db withSession {
      autoInc.insert(entity)
    }
  }

  protected def update(entity: T) = db withSession {
    this.where(_.id === entity.id).update(entity)
  }
}