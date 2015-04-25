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
 * Generic access to the data source.
 *
 * @author andreas
 * @version 0.0.4, 2015-04-25
 */
trait GenericDao[T <: Entity] {

  self: Table[T] =>

  // access to the data source
  implicit val db = Database.forDataSource(DB.getDataSource())

  /**
   * Abstract method to ensure every item has an identifier column.
   *
   * Note, since [[models.Entity]]s are stored here, each item automatically has an <em>id</em> column.
   */
  def id: Column[Long]

  /**
   * Abstract method that requires each passed in item to implement the * method.
   */
  def * : scala.slick.lifted.ColumnBase[T]

  /**
   * Abstract method to provide for an insertion without identifier.
   *
   * The tuple that is passed needs to contain all the fields of the entity in the right order
   * except the identifier <em>id</em>.
   */
  def withoutId: slick.driver.PostgresDriver.KeysInsertInvoker[_, Long]

  /**
   * Load the instance of the type in question that is associated with the specified <em>id</em>
   *
   * @param id The identifier of the instance to get
   * @return The instance matching the identifier or [[scala.None]]
   */
  def get(id: Long): Validation[Throwable, Option[T]] = db withSession {
    try {
      Success(Query(this).filter(_.id === id).firstOption)
    }
    catch {
      case t: Throwable => Failure(t)
    }
  }

  /**
   * Retrieve all items of the given tpye.
   *
   * @return a [[scalaz.Validation]] that either contains a possibly empty list of items of the given
   *         type or the [[scala.Throwable]] that occurred when trying to load.
   */
  def getAll: Validation[Throwable, List[T]] = db withSession {
    try {
      def q = Query(this).sortBy(t => t.id).list
      Success(q)
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  /**
   * Update an existing item in the database
   *
   * Note, it is recommended to update the <em>modified</em> field before sending the entity here
   * because the item is flushed to the database without any further action.
   *
   * @param entity The item to be updated.
   * @return The number of rows that were affected by the operation.
   */
  protected def update(entity: T) = db withSession {
    Query(this).filter(_.id === entity.id).update(entity)
  }

  /**
   * Remove the item specified by <em>id</em> from the database.
   *
   * @param id The identifier of the item to be removed.
   * @return The number of rows that have been affected by the operation.
   */
  def delete(id: Long): Int = db withSession {
    Query(this).filter(_.id === id).delete
  }

  /**
   * Count the number of items of the given type.
   *
   * @return The number of items currently persisted in the database.
   */
  def count = db withSession {
    Query(this).countDistinct
  }
}