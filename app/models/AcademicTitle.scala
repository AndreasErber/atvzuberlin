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
import scalaz.Failure
import scalaz.Success
import scalaz.Validation

/**
 * @author andreas
 * @version 0.0.2, 2013-04-21
 */
case class AcademicTitle(
  override val id: Option[Long] = None,
  val abbr: String,
  val maleForm: Option[String],
  val femaleForm: Option[String],
  override val created: Long = System.currentTimeMillis(),
  override val creator: String,
  override val modified: Option[Long] = None,
  override val modifier: Option[String]) extends Entity(id, created, creator, modified, modifier) {

  /**
   * Redefine the inherited method to limit equality to instances of the same type.
   *
   * @param other The instance to compare this instance with.
   * @return <code>true</code> if the other instance is of the same type, <code>false</code> otherwise.
   */
  override def canEqual(other: Any) = other.isInstanceOf[AcademicTitle]

  /**
   * Redefine the comparison function.
   *
   * @param other The instance to compare to this instance.
   * @return <code>true</code> if the instance are the same or equal, <code>false</code> otherwise.
   */
  override def equals(other: Any): Boolean = other match {
    case that: AcademicTitle =>
      if (this eq that) true
      else {
        (that.## == this.##) &&
          (that canEqual this) &&
          (this.abbr == that.abbr) &&
          (this.maleForm == that.maleForm) &&
          (this.femaleForm == that.femaleForm)
      }
    case _ => false
  }

  /**
   * Redefine the hashing function.
   *
   * @return The hash of this object.
   */
  override def hashCode(): Int = {

    var hash = 17
    hash = 31 * hash + this.abbr.hashCode
    hash = 31 * hash + this.maleForm.hashCode
    hash = 31 * hash + this.femaleForm.hashCode
    hash
  }
}

object AcademicTitle {
  implicit lazy val db = Database.forDataSource(DB.getDataSource())
  val tablename = "AcademicTitle"

  def delete(id: Long): Validation[Throwable, Boolean] = {
    try {
      val delCount = Query(AcademicTitles).filter(_.id === id).delete
      if (delCount > 0) Success(true) else Failure(new RuntimeException("Failed to delete academic title with ID " + id))
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  /**
   * Get the {@link AcademicTitle} identified by <em>id</em>.
   */
  def getAcademicTitle(id: Long): Validation[Throwable, Option[AcademicTitle]] = {
    db withSession {
      try {
        val ohe = Query(AcademicTitles).where(_.id === id).firstOption
        if (ohe.isDefined) {
          Success(ohe)
        } else {
          Failure(new RuntimeException("Academic title with ID " + id + " not found."))
        }
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }
  
  /**
   * Retrieve all {@link AcademicTitle}s from the persistence store.
   */
  def getAll(): Validation[Throwable, List[AcademicTitle]] = db withSession {
    try {
      Success(Query(AcademicTitles).list)
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  def getPersonTitles(p: Person): Validation[Throwable, List[AcademicTitle]] = db withSession {
    require(Option(p).isDefined)
    val phts = PersonHasTitle.getAllForPerson(p)
    if (phts.isSuccess) {
    	val titles = for (pht <- phts.toOption.get) yield (AcademicTitle.load(pht.tid).get)
    	Success(titles)
    } else {
      Failure(phts.fail.toOption.get)
    }
  }
  
  def load(id: Long): Option[AcademicTitle] = db withSession {
    Query(AcademicTitles).filter(_.id === id).firstOption
  }

  /**
   * Persist a new {@link AcademicTitle} instance or update an existing one.
   */
  def saveOrUpdate(at: AcademicTitle): Validation[Throwable, AcademicTitle] = {
    db withSession {

      require(Option(at).isDefined)
      // if the object has an identifier it is an update
      if (at.id.isDefined) {
        try {
          val count = AcademicTitles.update(at)
          if (count == 0) {
            Failure(new RuntimeException("Failed to update academic title " + at))
          } else {
            Success(at)
          }
        } catch {
          case e: Throwable => Failure(e)
        }
      } // objects without identifier are new and must be inserted
      else {
        try {
          val id = AcademicTitles.insert(at)
          Success(at.copy(id = Some(id)))
        } catch {
          case e: Throwable => Failure(e)
        }
      }
    }
  }
}

object AcademicTitles extends Table[AcademicTitle](AcademicTitle.tablename) {

  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def abbr = column[String]("abbr")
  def maleForm = column[String]("maleForm", O.Nullable)
  def femaleForm = column[String]("femaleForm", O.Nullable)
  def created = column[Long]("created")
  def creator = column[String]("creator")
  def modified = column[Long]("modified", O.Nullable)
  def modifier = column[String]("modifier", O.Nullable)
  def * = id.? ~ abbr ~ maleForm.? ~ femaleForm.? ~ created ~ creator ~ modified.? ~ modifier.? <> (AcademicTitle.apply _, AcademicTitle.unapply _)

  def withoutId = abbr ~ maleForm.? ~ femaleForm.? ~ created ~ creator ~ modified.? ~ modifier.? returning id
  def insert = (at: AcademicTitle) => withoutId.insert(at.abbr, at.maleForm, at.femaleForm, at.created, at.creator, at.modified, at.modifier)
  def update(at: AcademicTitle): Int = AcademicTitles.where(_.id === at.id).update(at.copy(modified = Some(System.currentTimeMillis())))
}