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

/**
 * Entity to represent a news item.
 *
 * @author andreas
 * @version 0.0.2, 2015-04-20
 */
case class News(override val id: Option[Long] = None,
                title: String,
                lead: Option[String],
                content: Option[String],
                override val created: Long = System.currentTimeMillis(),
                override val creator: String,
                override val modified: Option[Long] = None,
                override val modifier: Option[String] = None) extends Entity(id, created, creator, modified, modifier)

object News {

  implicit lazy val db = Database.forDataSource(DB.getDataSource())
  val tablename = "News"

  /**
   * Retrieve all news items from the persistence store.
   */
  def getAll(limit: Option[Int]): Validation[Throwable, List[News]] = db withSession {
    def q = Query(NewsTable).sortBy(n => n.created).list.reverse
    try {
      Success(if (limit.isDefined) q.take(limit.get) else q)
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  /**
   * Load the news item related to the given identifier.
   */
  def load(id: Long): Validation[Throwable, Option[News]] = db withSession {
    try {
      Success(Query(NewsTable).filter(_.id === id).firstOption)
    } catch {
      case t: Throwable => Failure(t)
    }
  }

  /**
   * Persist a new news item or update an existing one.
   */
  def saveOrUpdate(n: News): Validation[Throwable, News] = {
    db withSession {

      require(Option(n).isDefined)
      // if the object has an identifier it is an update
      if (n.id.isDefined) {
        try {
          val count = NewsTable.update(n)
          if (count == 0) {
            Failure(new RuntimeException("Failed to update news item " + n))
          } else {
            Success(n)
          }
        } catch {
          case e: Throwable => Failure(e)
        }
      } // objects without identifier are new and must be inserted
      else {
        try {
          val id = NewsTable.insert(n)
          Success(n.copy(id = Some(id)))
        } catch {
          case e: Throwable => Failure(e)
        }
      }
    }
  }

  /**
   * Delete the news item identified by id.
   */
  def delete(id: Long): Validation[Throwable, Boolean] = db withSession {
    try {
      val delCount = Query(NewsTable).filter(_.id === id).delete
      if (delCount > 0) Success(true) else Failure(new RuntimeException("Failed to delete news item with id " + id))
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  /**
   * Count the number of news items.
   */
  def count(): Int = db withSession {
    NewsTable.count
  }
}

object NewsTable extends Table[News](News.tablename) {

  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

  def title = column[String]("title")

  def lead = column[String]("lead", O.Nullable)

  def content = column[String]("content", O.Nullable, O.DBType("text"))

  def created = column[Long]("created")

  def creator = column[String]("creator")

  def modified = column[Long]("modified", O.Nullable)

  def modifier = column[String]("modifier", O.Nullable)

  def * = id.? ~ title ~ lead.? ~ content.? ~ created ~ creator ~ modified.? ~ modifier.? <>(News.apply _, News.unapply _)

  def withoutId = title ~ lead.? ~ content.? ~ created ~ creator ~ modified.? ~ modifier.? returning id

  def insert = (n: News) => withoutId.insert(n.title, n.lead, n.content, n.created, n.creator, n.modified, n.modifier)

  def update(n: News): Int = NewsTable.where(_.id === n.id).update(n.copy(modified = Some(System.currentTimeMillis())))

  def count(): Int = NewsTable.count
}