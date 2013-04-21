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
import util.UsageType
import util.Privacy
import util.Personal
import util.MembersPrivate

/**
 * @author andreas
 * @verson 0.0.2, 2013-04-21
 */
case class PersonHasTitle(pid: Long, tid: Long, pos: Int)

object PersonHasTitle {

  implicit lazy val db = Database.forDataSource(DB.getDataSource())
  val tablename = "PersonHasTitle"

  /**
   * Add a {@link PersonHasTitle} for the {@link Person} <em>p</em> and the {@link AcademicTitle} <em>at</em>.
   * It is appended to the list of titles the person has. If the {@link Person} <em>p</em> is already linked to
   * the {@link AcademicTitle} <em>at</em> an error is indicated.
   *
   *  @param p The {@link Person} instance that owns the {@link AcademicTitle} <em>at</em>.
   *  @param at The {@link AcademicTitle} the {@link Person} <em>p</em> owns.
   *  @returns A {@link Validation} of the newly created {@link PersonHasTitle} or a {@link Throwable} in case of error.
   */
  def add(p: Person, at: AcademicTitle): Validation[Throwable, PersonHasTitle] = db withSession {

    require(Option(p).isDefined && Option(at).isDefined)
    try {
      val phts = getAllForPerson(p)
      if (phts.isSuccess) {
        if (!phts.toOption.get.isEmpty) {
          for (pht <- phts.toOption.get) {
            pht match {
              case PersonHasTitle(p.id, at.id, _) =>
                return Failure(new RuntimeException("Person already has this title set"))
              case _ =>
            }
          }
        }
        // here, the list was either empty or no match was found
        try {
          val pht = PersonHasTitle(p.id.get, at.id.get, phts.toOption.get.length + 1)
          PersonHasTitles.insert(pht)
          Success(pht)
        } catch {
          case e: Throwable => Failure(e)
        }
      } else {
        Failure(phts.fail.toOption.get)
      }
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  /**
   * Switches the positions of the two {@link PersonHasTitle} instances.
   *
   * @param at1 The {@link PersonHasTitle} instance that gets repositioned onto the position of {@link PersonHasTitle} <em>at2</em>
   * @param at2 The {@link PersonHasTitle} instance that gets repositioned onto the position of {@link PersonHasTitle} <em>at1</em>
   * @return A {@link Pair} of the two resulting {@link PersonHasTitle} instances where the corresponding copy of <em>at1</em> is on
   * the first position  or a {@link Throwable} in case of error.
   */
  def switch(at1: PersonHasTitle, at2: PersonHasTitle): Validation[Throwable, Pair[PersonHasTitle, PersonHasTitle]] = db withSession {
    require(Option(at1).isDefined && Option(at2).isDefined)
    val at1a = at1.copy(pos = at2.pos)
    val at2a = at2.copy(pos = at1.pos)

    try {
      PersonHasTitles.update(at1)
      PersonHasTitles.update(at2)
      Success(Pair(at1a, at2a))
    } catch {
      case e: Throwable => Failure(e)
    }

  }

  def getAllForPerson(p: Person): Validation[Throwable, List[PersonHasTitle]] = db withSession {
    getAllForPerson(p.id.get)
  }

  private def getAllForPerson(pid: Long): Validation[Throwable, List[PersonHasTitle]] = db withSession {
    try {
      Success(Query(PersonHasTitles).where(_.pid === pid).list)
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  /**
   * Persist a new {@link AcademicTitle} instance or update an existing one.
   *
   * @FIXME needs to cover all possible update Situations
   */
  def saveOrUpdate(at: PersonHasTitle): Validation[Throwable, PersonHasTitle] = {
    db withSession {

      require(Option(at).isDefined)
      val phts = getAllForPerson(at.pid)
      if (phts.isSuccess) {
        // check a non-empty result for a match
        if (!phts.toOption.get.isEmpty) {
          for (pht <- phts.toOption.get) {
            pht match {
              case PersonHasTitle(at.pid, at.tid, _) =>
                try {
                  PersonHasTitles.update(at)
                  return Success(at)
                } catch {
                  case e: Throwable => Failure(e)
                }
            }
          }
        }
        // here, the list was either empty or no match was found
        try {
          PersonHasTitles.insert(at)
          Success(at)
        } catch {
          case e: Throwable => Failure(e)
        }
      } else {
        Failure(phts.fail.toOption.get)
      }
    }
  }

}

object PersonHasTitles extends Table[PersonHasTitle](PersonHasTitle.tablename) {

  def pid = column[Long]("pid")
  def tid = column[Long]("tid")
  def pos = column[Int]("position")
  def * = pid ~ tid ~ pos <> (PersonHasTitle.apply _, PersonHasTitle.unapply _)
  def person = foreignKey("person_fk", pid, Persons)(_.id)
  def email = foreignKey("title_fk", tid, AcademicTitles)(_.id)
  def pk = primaryKey("pk_personhastitle", (pid, tid))
  def update(pht: PersonHasTitle): Int = PersonHasTitles.where(_.pid === pht.pid).where(_.tid === pht.tid).update(pht)
}