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
import scalaz.Validation
import scalaz.Failure
import scalaz.Success
import util.Division

/**
 * Class to define a charge or duty that is to be taken by a person.
 *
 * @author andreas
 * @version 0.0.2, 2015-01-02
 */
case class Charge(override val id: Option[Long],
  val name: String,
  val abbr: Option[String],
  val division: Division.Division = Division.Aktivitas,
  val shortDesc: Option[String],
  val longDesc: Option[String],
  val email: Option[String],
  override val created: Long = System.currentTimeMillis(),
  override val creator: String,
  override val modified: Option[Long] = None,
  override val modifier: Option[String]) extends Entity(id, created, creator, modified, modifier) {

}

/**
 * Companion object for the {@link Charge} class.
 *
 * Contains database access methods.
 *
 * @author andreas
 * @version 0.0.1, 2013-07-16
 */
object Charge {

  implicit val db = Database.forDataSource(DB.getDataSource())
  val tablename = "Charge"

  /**
   * Retrieve all charges from the persistence store.
   */
  def getAll(): Validation[Throwable, List[Charge]] = db withSession {
    def q = Query(Charges).sortBy(n => n.name).list
    try {
      Success(q)
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  def getAllForDivision(div: Division.Division): Validation[Throwable, List[Charge]] = db withSession {
    val all = this.getAll
    if (all.isFailure) {
      all
    } else {
      val list = all.toOption.get
      Success(list.filter(_.division == div))
    }
  }

  /**
   * Load the charge related to the given identifier.
   *
   * @param id The identifier of the charge in question.
   */
  def load(id: Long): Option[Charge] = db withSession {
    Query(Charges).filter(_.id === id).firstOption
  }

  /**
   * Persist a new charge or update an existing one.
   *
   * @param c The charge that is to be persisted
   */
  def saveOrUpdate(c: Charge): Validation[Throwable, Charge] = {
    db withSession {

      require(Option(c).isDefined)
      // if the object has an identifier it is an update
      if (c.id.isDefined) {
        try {
          val count = Charges.update(c)
          if (count == 0) {
            Failure(new RuntimeException("Failed to update charge " + c))
          } else {
            Success(c)
          }
        } catch {
          case e: Throwable => Failure(e)
        }
      } // objects without identifier are new and must be inserted
      else {
        try {
          val id = Charges.insert(c)
          Success(c.copy(id = Some(id)))
        } catch {
          case e: Throwable => Failure(e)
        }
      }
    }
  }

  /**
   * Delete the charge identified by id.
   *
   * @param id The identifier of the charge to be deleted.
   * @return A {@link Validation} with either a {@link Throwable} or a {@link Boolean}. If the
   *         action itself was successful (could be executed without error), the Boolean indicates
   *         if the item was successfully deleted (<code>true</code>) or not (<code>false</code>).
   */
  def delete(id: Long): Validation[Throwable, Boolean] = db withSession {
    try {
      val delCount = Query(Charges).filter(_.id === id).delete
      if (delCount > 0) Success(true) else Failure(new RuntimeException("Failed to delete charge with id " + id))
    } catch {
      case e: Throwable => Failure(e)
    }
  }
}

/**
 * The data access object for {@link Charge}s.
 *
 * @author andreas
 * @version 0.0.1, 2013-07-16
 */
object Charges extends Table[Charge](Charge.tablename) {

  import scala.slick.lifted.MappedTypeMapper.base
  import scala.slick.lifted.TypeMapper
  implicit val divisionMapper: TypeMapper[Division.Division] = base[Division.Division, String](d => d.toString, string => Division.withName(string))

  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name")
  def abbr = column[String]("abbr", O.Nullable)
  def division = column[Division.Division]("division")
  def shortDesc = column[String]("shortDesc", O.Nullable)
  def longDesc = column[String]("longDesc", O.Nullable, O.DBType("text"))
  def email = column[String]("email", O.Nullable)
  def created = column[Long]("created")
  def creator = column[String]("creator")
  def modified = column[Long]("modified", O.Nullable)
  def modifier = column[String]("modifier", O.Nullable)
  def * = id.? ~ name ~ abbr.? ~ division ~ shortDesc.? ~ longDesc.? ~ email.? ~ created ~ creator ~ modified.? ~ modifier.? <> (Charge.apply _, Charge.unapply _)

  def withoutId = name ~ abbr.? ~ division ~ shortDesc.? ~ longDesc.? ~ email.? ~ created ~ creator ~ modified.? ~ modifier.? returning id
  def insert = (c: Charge) => withoutId.insert(c.name, c.abbr, c.division, c.shortDesc, c.longDesc, c.email, c.created, c.creator, c.modified, c.modifier)
  def update(c: Charge): Int = Charges.where(_.id === c.id).update(c.copy(modified = Some(System.currentTimeMillis())))
}