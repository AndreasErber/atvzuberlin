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

/**
 * @author andreas
 * @version 0.0.3, 2015-01-03
 */
case class Homepage(override val id: Option[Long],
    val url: String,
    val descr: Option[String],
    override val created: Long = System.currentTimeMillis(),
    override val creator: String,
    override val modified: Option[Long] = None,
    override val modifier: Option[String]) extends Entity(id, created, creator, modified, modifier)

object Homepage {
  
  implicit val db = Database.forDataSource(DB.getDataSource())
  val tablename = "Homepage"

  def load(id: Long): Option[Homepage] = db withSession {
    Query(Homepages).filter(_.id === id).firstOption
  }

  private def delete(id: Long): Validation[Throwable, Boolean] = {
    try {
      val delCount = Query(Homepages).filter(_.id === id).delete
      if (delCount > 0) Success(true) else Failure(new RuntimeException("Failed to delete homepage with id " + id))
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  def deleteOrgHomepage(oid: Long, id: Long): Validation[Throwable, Boolean] = db withSession {
    val del = Query(OrgHasHomepages).where(_.oid === oid).where(_.hid === id).delete
    if (del > 0) {
      delete(id)
    } else {
      Failure(new RuntimeException("Failed to delete the connection between the organization and the homepage. (" + oid + ", " + id + ")"))
    }
  }

  def deletePersonHomepage(pid: Long, id: Long): Validation[Throwable, Boolean] = db withSession {
    val del = Query(PersonHasHomepages).where(_.pid === pid).where(_.hid === id).delete
    if (del > 0) {
      delete(id)
    } else {
      Failure(new RuntimeException("Failed to delete the connection between the person and the homepage. (" + pid + ", " + id + ")"))
    }
  }

  def findByUrl(url: String): Validation[Throwable, Option[Homepage]] = {
    db withSession {
      try {
        val result = Query(Homepages).where(_.url === url).firstOption
        result match {
          case None => Failure(new RuntimeException("Failed to find homepage with URL " + url))
          case _    => Success(result)
        }
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }

  /**
   * Get the {@link Homepage} identified by <em>eid</em> for the {@link Organization} <em>o</em>.
   */
  def getOrgHomepage(o: Organization, hid: Long): Validation[Throwable, Option[(Homepage, OrgHasHomepage)]] = {
    require(o.id.isDefined)
    db withSession {
      try {
        val ohh = Query(OrgHasHomepages).where(_.oid === o.id.get).where(_.hid === hid).firstOption
        val hp = load(hid)
        if (ohh.isDefined) {
          if (hp.isDefined) {
            Success(Some(hp.get, ohh.get))
          } else {
            Failure(new RuntimeException("Homepage with ID " + hid + " not found."))
          }
        } else {
          Success(None)
        }
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }

  /**
   * Get the list of {@link Homepage}s an {@link Organization} has.
   *
   * Note, that the output list is sorted according to the position parameter in the relation table.
   */
  def getOrgHomepages(o: Organization): Validation[Throwable, List[(Homepage, OrgHasHomepage)]] = {
    require(o.id.isDefined)
    db withSession {
      try {
        val result = for {
          ohh <- OrgHasHomepages.sortBy(x => (x.oid, x.pos)) if ohh.oid === o.id.get
          h <- Homepages if ohh.hid === h.id
        } yield ((h, ohh))
        Success(result.list)
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }

  /**
   * Get the {@link Homepage} identified by <em>eid</em> for the {@link Person} <em>p</em>.
   */
  def getPersonHomepage(p: Person, hid: Long): Validation[Throwable, Option[(Homepage, PersonHasHomepage)]] = {
    require(p.id.isDefined)
    db withSession {
      try {
        val phh = Query(PersonHasHomepages).where(_.pid === p.id.get).where(_.hid === hid).firstOption
        val hp = load(hid)
        if (phh.isDefined) {
          if (hp.isDefined) {
            Success(Some((hp.get, phh.get)))
          } else {
            Failure(new RuntimeException("Homepage with ID " + hid + " not found."))
          }
        } else {
          Success(None)
        }
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }

  /**
   * Get the list of {@link Homepage} a {@link Person} has.
   *
   * Note, that the output list is sorted according to the position parameter in the relation table.
   */
  def getPersonHomepages(p: Person): Validation[Throwable, List[(Homepage, PersonHasHomepage)]] = {
    require(p.id.isDefined)
    db withSession {
      try {
        val result = for {
          phh <- PersonHasHomepages.sortBy(x => (x.pid, x.pos)) if phh.pid === p.id.get
          hp <- Homepages if phh.hid === hp.id
        } yield ((hp, phh))
        Success(result.list)
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }

  /**
   * Insert the specified {@link Homepage} item in the database and set it into a relation to the given {@link Person}.
   *
   * If the person is not yet persisted it will be in a first step.
   */
  def savePersonHomepage(p: Person, hp: Homepage, u: UsageType, pos: Int = 0): Validation[Throwable, (Homepage, PersonHasHomepage)] = {

    var p1 = p
    if (!p.id.isDefined) {
      p1 = Person.saveOrUpdate(p).toOption.get
    }
    db withSession {
      try {
        val isUpdate = if (hp.id.isDefined) true else false
        val hp1 = Homepage.saveOrUpdate(hp)
        if (hp1.isSuccess) {
          var position = -1
          if (!isUpdate) {
            val l = Query(PersonHasHomepages).where(_.pid === p1.id.get).list
            position = l.length +1
            PersonHasHomepages.insert(PersonHasHomepage(p1.id.get, hp1.toOption.get.id.get, position));
          } else {
            val oldPos = Query(PersonHasHomepages).where(_.pid === p1.id.get).where(_.hid === hp1.toOption.get.id.get).firstOption.get.pos
            position = if (pos == 0) oldPos else pos
            PersonHasHomepages.update(PersonHasHomepage(p1.id.get, hp1.toOption.get.id.get, position, u))
            if (oldPos > position) {
              val l = Query(PersonHasHomepages).where(_.pid === p1.id.get).where(_.pos >= position).list.sortBy(x => x.pos)
              for (phh1 <- l) {
                if (phh1.hid != hp1.toOption.get.id.get && phh1.pos > oldPos && phh1.pos <= position) {
                  PersonHasHomepages.update(phh1.copy(pos = phh1.pos + 1))
                }
              }
            } else if (oldPos < position) {
              val l = Query(PersonHasHomepages).where(_.pid === p1.id.get).where(_.pos <= position).list.sortBy(x => x.pos)
              for (phh1 <- l) {
                if (phh1.hid != hp1.toOption.get.id.get && phh1.pos < oldPos && phh1.pos >= position) {
                  PersonHasHomepages.update(phh1.copy(pos = phh1.pos - 1))
                }
              }
            }
          }
          Success((hp1.toOption.get, PersonHasHomepage(p.id.get, hp1.toOption.get.id.get, position, u)))
        } else Failure(hp1.toEither.left.get)
      } catch {
        case e: Throwable => Failure(e)
      }

    }
  }

  /**
   * Insert the specified {@link Homepage} item in the database and set it into a relation to the given {@link Organization}.
   *
   * If the person is not yet persisted it will be in a first step.
   */
  def saveOrgHomepage(o: Organization, hp: Homepage, pos: Int = 0): Validation[Throwable, (Homepage, OrgHasHomepage)] = {

    var o1 = o
    if (!o.id.isDefined) {
      o1 = Organization.saveOrUpdate(o).toOption.get
    }
    db withSession {
      try {
        val isUpdate = if (hp.id.isDefined) true else false
        val hp1 = Homepage.saveOrUpdate(hp)
        if (hp1.isSuccess) {
          var position = -1
          if (!isUpdate) {
            val l = Query(OrgHasHomepages).where(_.oid === o1.id.get).list
            position = l.length + 1
            OrgHasHomepages.insert(OrgHasHomepage(o1.id.get, hp1.toOption.get.id.get, position));
          } else {
            val oldPos = Query(OrgHasHomepages).where(_.oid === o1.id.get).where(_.hid === hp1.toOption.get.id.get).firstOption.get.pos
            position = if (pos == 0) oldPos else pos
            OrgHasHomepages.update(OrgHasHomepage(o.id.get, hp1.toOption.get.id.get, position))
            if (oldPos > position) {
              val l = Query(OrgHasHomepages).where(_.oid === o1.id.get).where(_.pos >= position).list.sortBy(x => x.pos)
              for (ohh1 <- l) {
                if (ohh1.hid != hp1.toOption.get.id.get && ohh1.pos > oldPos && ohh1.pos <= hp1.toOption.get.id.get) {
                  OrgHasHomepages.update(ohh1.copy(pos = ohh1.pos + 1))
                }
              }
            } else if (oldPos < position) {
              val l = Query(OrgHasHomepages).where(_.oid === o1.id.get).where(_.pos <= position).list.sortBy(x => x.pos)
              for (ohh1 <- l) {
                if (ohh1.hid != hp1.toOption.get.id.get && ohh1.pos < oldPos && ohh1.pos >= hp1.toOption.get.id.get) {
                  OrgHasHomepages.update(ohh1.copy(pos = ohh1.pos - 1))
                }
              }
            }
          }
          Success((hp1.toOption.get, OrgHasHomepage(o.id.get,hp1.toOption.get.id.get, position)))
        } else Failure(hp1.toEither.left.get)
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }

  private def saveOrUpdate(h: Homepage): Validation[Throwable, Homepage] = {
    db withSession {
      if (h.id.isDefined) {
        try {
          val count = Homepages.update(h)
          if (count == 0) {
            Failure(new RuntimeException("Failed to update homepage " + h))
          } else {
            Success(h)
          }
        } catch {
          case e: Throwable => Failure(e)
        }
      } else {
        try {
          val id = Homepages.insert(h)
          Success(h.copy(id = Some(id)))
        } catch {
          case e: Throwable => Failure(e)
        }
      }
    }
  }
}

object Homepages extends Table[Homepage](Homepage.tablename) {

  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def url = column[String]("url")
  def descr = column[String]("descr", O.Nullable)
  def created = column[Long]("created")
  def creator = column[String]("creator")
  def modified = column[Long]("modified", O.Nullable)
  def modifier = column[String]("modifier", O.Nullable)
  def * = id.? ~ url ~ descr.? ~ created ~ creator ~ modified.? ~ modifier.? <> (Homepage.apply _, Homepage.unapply _)

  def withoutId = url ~ descr.? ~ created ~ creator ~ modified.? ~ modifier.? returning id
  def insert = (h: Homepage) => withoutId.insert(h.url, h.descr, h.created, h.creator, h.modified, h.modifier)
  def update(h: Homepage): Int = Homepages.where(_.id === h.id).update(h.copy(modified = Some(System.currentTimeMillis())))
}
