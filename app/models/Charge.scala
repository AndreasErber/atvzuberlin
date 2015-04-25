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
import util.Division

/**
 * Class to define a [[Charge]] or duty that is to be taken by a person.
 *
 * @author andreas
 * @version 0.0.4, 2015-04-24
 */
case class Charge(override val id: Option[Long],
                  nameMale: String,
                  nameFemale: String,
                  abbr: Option[String],
                  division: Division.Division = Division.Aktivitas,
                  position: Int,
                  shortDesc: Option[String],
                  longDesc: Option[String],
                  emailMale: Option[String],
                  emailFemale: Option[String],
                  override val created: Long = System.currentTimeMillis(),
                  override val creator: String,
                  override val modified: Option[Long] = None,
                  override val modifier: Option[String]) extends Entity(id, created, creator, modified, modifier) {
}

/**
 * Companion object for the [[Charge]] class.
 *
 * Contains database access methods.
 *
 * @author andreas
 * @version 0.0.2, 2015-04-18
 */
object Charge {

  implicit val db = Database.forDataSource(DB.getDataSource())
  val tablename = "Charge"

  /**
   * Retrieve all [[Charge]]s from the persistence store.
   *
   * @return A [[Validation]] of either a [[Throwable]] or a [[List]] of [[Charge]]s.
   */
  def getAll: Validation[Throwable, List[Charge]] = db withSession {
    def q = Query(Charges).sortBy(n => n.nameMale).list
    try {
      Success(q)
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  /**
   * Retrieve all [[Charge]]s for a certain [[Division]].
   *
   * @param div The [[Division]] to get the [[Charge]]s for.
   * @return A [[Validation]] of either a [[Throwable]] or a [[List]] of [[Charge]]s.
   */
  def getAllForDivision(div: Division.Division): Validation[Throwable, List[Charge]] = db withSession {
    try {
      val all = this.getAll
      if (all.isFailure) {
        all
      } else {
        val list = all.toOption.get
        Success(list.filter(_.division == div))
      }
    } catch {
      case t: Throwable => Failure(t)
    }
  }

  /**
   * Load the [[Charge]] related to the given identifier.
   *
   * @param id The identifier of the [[Charge]] in question.
   * @return A [[Validation]] of either a [[Throwable]] or an optional [[Charge]].
   */
  def load(id: Long): Validation[Throwable, Option[Charge]] = db withSession {
    try {
      Success(Query(Charges).filter(_.id === id).firstOption)
    } catch {
      case t: Throwable => Failure(t)
    }
  }

  /**
   * Persist a new [[Charge]] or update an existing one.
   *
   * @param c The [[Charge]] that is to be persisted or updated
   * @return A [[Validation]] of either a [[Throwable]] or a [[Charge]].
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
   * @return A [[Validation]] with either a [[Throwable]] or a [[Boolean]]. If the
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
 * The data access object for [[Charge]]s.
 *
 * @author andreas
 * @version 0.0.1, 2013-07-16
 */
object Charges extends Table[Charge](Charge.tablename) {

  import scala.slick.lifted.MappedTypeMapper.base
  import scala.slick.lifted.TypeMapper

  implicit val divisionMapper: TypeMapper[Division.Division] = base[Division.Division, String](d => d.toString, string => Division.withName(string))

  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

  def nameMale = column[String]("nameMale")

  def nameFemale = column[String]("nameFemale")

  def abbr = column[String]("abbr", O.Nullable)

  def division = column[Division.Division]("division")

  def position = column[Int]("position")

  def shortDesc = column[String]("shortDesc", O.Nullable)

  def longDesc = column[String]("longDesc", O.Nullable, O.DBType("text"))

  def emailMale = column[String]("emailMale", O.Nullable)

  def emailFemale = column[String]("emailFemale", O.Nullable)

  def created = column[Long]("created")

  def creator = column[String]("creator")

  def modified = column[Long]("modified", O.Nullable)

  def modifier = column[String]("modifier", O.Nullable)

  def * = id.? ~ nameMale ~ nameFemale ~ abbr.? ~ division ~ position ~ shortDesc.? ~ longDesc.? ~ emailMale.? ~
    emailFemale.? ~ created ~ creator ~ modified.? ~ modifier.? <>(Charge.apply _, Charge.unapply _)

  def withoutId = nameMale ~ nameFemale ~ abbr.? ~ division ~ position ~ shortDesc.? ~ longDesc.? ~ emailMale.? ~
    emailFemale.? ~ created ~ creator ~ modified.? ~ modifier.? returning id

  def insert = (c: Charge) => withoutId.insert(c.nameMale, c.nameFemale, c.abbr, c.division, c.position, c.shortDesc, c
    .longDesc, c.emailMale, c.emailFemale, c.created, c.creator, c.modified, c.modifier)

  def update(c: Charge): Int = Charges.where(_.id === c.id).update(c.copy(modified = Some(System.currentTimeMillis())))
}