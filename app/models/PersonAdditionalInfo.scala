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
import java.sql.Date
import java.text.DateFormat
import java.util.Calendar
import db.GenericDao
import util.{Aktiv, Bbr, FormOfAddress, LetterSalutation, MemberState, Mr}

/**
 * Additional information about a [[Person]]
 *
 * @author andreas
 * @version 0.0.5, 2015-04-20
 */
case class PersonAdditionalInfo(
                                 private val pid: Long,
                                 formOfAddress: FormOfAddress,
                                 letterSalutation: LetterSalutation,
                                 enlistment: Option[Date] = None,
                                 withdrawal: Option[Date] = None,
                                 status: MemberState,
                                 profession: Option[String] = None,
                                 employer: Option[String] = None,
                                 override val created: Long = System.currentTimeMillis(),
                                 override val creator: String,
                                 override val modified: Option[Long] = None,
                                 override val modifier: Option[String] = None) extends Entity(Some(pid), created, creator, modified, modifier) {

  def enlistmentFormatted(df: DateFormat): String = {
    require(Option(df).isDefined)
    val cal = Calendar.getInstance()
    if (this.enlistment.isDefined) {
      cal.setTimeInMillis(this.enlistment.get.getTime)
      df.format(cal.getTime)
    } else ""
  }

  def withdrawalFormatted(df: DateFormat): String = {
    require(Option(df).isDefined)
    val cal = Calendar.getInstance()
    if (this.withdrawal.isDefined) {
      cal.setTimeInMillis(this.withdrawal.get.getTime)
      df.format(cal.getTime)
    } else ""
  }
}

object PersonAdditionalInfo {
  implicit lazy val db = Database.forDataSource(DB.getDataSource())
  val tablename = "PersonAdditionalInfo"

  /**
   * Delete the [[PersonAdditionalInfo]] identified by id.
   */
  def delete(id: Long): Validation[Throwable, Boolean] = db withSession {
    try {
      val delCount = Query(PersonAdditionalInfos).filter(_.id === id).delete
      if (delCount > 0) Success(true) else Failure(new RuntimeException("Failed to delete person additional info with ID " + id))
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  /**
   * Load the [[PersonAdditionalInfo]] related to the given identifier.
   */
  def load(id: Long): Validation[Throwable, Option[PersonAdditionalInfo]] = db withSession {
    try {
      Success(Query(PersonAdditionalInfos).filter(_.id === id).firstOption)
    } catch {
      case t: Throwable => Failure(t)
    }
  }

  /**
   * Persist a new [[PersonAdditionalInfo]] instance or update an existing one.
   */
  def saveOrUpdate(pai: PersonAdditionalInfo): Validation[Throwable, PersonAdditionalInfo] = {
    db withSession {
      require(Option(pai).isDefined)

      // if an entry with that ID already exists it is an update
      if (pai.id.isDefined) {
        try {
          val count = PersonAdditionalInfos.update(pai)
          if (count == 0) {
            Failure(new RuntimeException("Failed to update person additional info " + pai))
          } else {
            Success(pai)
          }
        } catch {
          case e: Throwable => Failure(e)
        }
      } // objects without identifier are new and must be inserted
      else {
        try {
          PersonAdditionalInfos.insert(pai)
          Success(pai)
        } catch {
          case e: Throwable => Failure(e)
        }
      }
    }
  }
}

object PersonAdditionalInfos extends Table[PersonAdditionalInfo](PersonAdditionalInfo.tablename) with GenericDao[PersonAdditionalInfo] {

  import scala.slick.lifted.MappedTypeMapper.base

  implicit val formOfAddressMapper = base[FormOfAddress, Int](foa => foa.id, id => Mr.getFormOfAddress(id).get)
  implicit val letterSalutationMapper = base[LetterSalutation, Int](ls => ls.id, id => Bbr.getLetterSalutation(id).get)
  implicit val memberStateMapper = base[MemberState, Int](ps => ps.id, id => Aktiv.getMemberState(id).get)

  def id = column[Long]("id", O.PrimaryKey)

  def formOfAddress = column[FormOfAddress]("formOfAddress")

  def letterSalutation = column[LetterSalutation]("letterSalutation")

  def enlistment = column[Date]("enlistment", O.Nullable)

  def withdrawal = column[Date]("withdrawal", O.Nullable)

  def status = column[MemberState]("status")

  def profession = column[String]("profession", O.Nullable)

  def employer = column[String]("employer", O.Nullable)

  def created = column[Long]("created")

  def creator = column[String]("creator")

  def modified = column[Long]("modified", O.Nullable)

  def modifier = column[String]("modifier", O.Nullable)

  def personFK = foreignKey("person_fk", id, Persons)(_.id)

  def * = id ~ formOfAddress ~ letterSalutation ~ enlistment.? ~ withdrawal.? ~ status ~ profession.? ~ employer.? ~ created ~ creator ~ modified.? ~ modifier.? <>(PersonAdditionalInfo.apply _, PersonAdditionalInfo.unapply _)

  def withoutId = throw new UnsupportedOperationException

  override def update(pai: PersonAdditionalInfo): Int = PersonAdditionalInfos.where(_.id === pai.id).update(pai.copy(modified = Some(System.currentTimeMillis())))

  /**
   * Retrieve all [[Person]] entries with the given <em>status</em>.
   */
  def getAllByStatus(status: MemberState): Validation[Throwable, List[Person]] = db withSession {
    try {
      val q = for {
        (p, pai) <- Persons innerJoin PersonAdditionalInfos on (_.id === _.id)
        if pai.status === status
      } yield p
      Success(q.list)
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  /**
   * Retrieve all entries having one of the given <em>status</em>.
   */
  def getAllByStatus(status: List[MemberState]): Validation[Throwable, List[Person]] = db withSession {
    try {
      val q = for {
        (p, pai) <- Persons innerJoin PersonAdditionalInfos on (_.id === _.id)
        if pai.status inSetBind status
      } yield p
      Success(q.list)
    } catch {
      case t: Throwable => Failure(t)
    }
  }
}