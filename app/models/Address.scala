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
import util.{Privacy, UsageType}

/**
 * Entity to provide a postal address.
 *
 * @author andreas
 * @version 0.0.6, 2015-04-18
 */
case class Address(override val id: Option[Long] = None,
                   addon: Option[String],
                   street: Option[String],
                   postbox: Option[String],
                   city: String,
                   zip: String,
                   country: Country,
                   override val created: Long = System.currentTimeMillis(),
                   override val creator: String,
                   override val modified: Option[Long] = None,
                   override val modifier: Option[String] = None) extends Entity(id, created, creator, modified, modifier)

object Address {

  implicit lazy val db = Database.forDataSource(DB.getDataSource())
  val tablename = "Address"

  /**
   * Retrieve all available [[Address]]es.
   *
   * @return A [[Validation]] of either a [[Throwable]] or a [[List]] of [[Address]]es.
   */
  def getAll: Validation[Throwable, List[Address]] = db withSession {
    try {
      Success(Query(Addresses).list)
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  /**
   * Retrieve the [[Address]] of a specific [[Organization]].
   *
   * @param o The [[Organization]] to get the [[Address]] for.
   * @param aid Identifier of the [[Address]]
   * @return A [[Validation]] of either a [[Throwable]] or an optional [[Address]].
   */
  def getOrgAddress(o: Organization, aid: Long): Validation[Throwable, Option[Address]] = {
    require(o.id.isDefined)
    db withSession {
      try {
        val oha = Query(OrgHasAddresses).where(_.oid === o.id.get).where(_.aid === aid).firstOption
        val adrV = load(aid)
        if (adrV.isSuccess) {
          if (oha.isDefined) {
            val adrOp: Option[Address] = adrV.toOption.get
            if (adrOp.isDefined) {
              Success(Some(adrOp.get))
            } else {
              Failure(new RuntimeException("Address with ID " + aid + " not found."))
            }
          } else {
            Success(None)
          }
        } else {
          throw adrV.toEither.left.get
        }
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }

  /**
   * Retrieve the [[Address]]es of a specific [[Organization]].
   *
   * @param o The [[Organization]] to get the [[Address]] for.
   * @return A [[Validation]] of either a [[Throwable]] or a [[List]] of [[Address]]es.
   */
  def getOrgAddresses(o: Organization): Validation[Throwable, List[Address]] = {
    require(o.id.isDefined)
    db withSession {
      try {
        val result = for {
          oha <- OrgHasAddresses.sortBy(x => (x.oid, x.pos)) if oha.oid === o.id.get
          a <- Addresses if oha.aid === a.id
        } yield a
        Success(result.list)
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }

  /**
   * Get the [[Address]] identified by <em>aid</em> the [[Person]] <em>p</em> has.
   *
   * @param p The [[Person]] to get the [[Address]] for.
   * @param aid Identifier of the [[Address]]
   * @return A [[Validation]] of either a [[Throwable]] or an optional 3-tuple of [[Address]], [[UsageType]], and
   *         [[Privacy]].
   */
  def getPersonAddress(p: Person, aid: Long): Validation[Throwable, Option[(Address, UsageType, Privacy)]] = {
    require(p.id.isDefined)
    db withSession {
      try {
        val pha = Query(PersonHasAddresses).where(_.pid === p.id.get).where(_.aid === aid).firstOption
        val adrV = load(aid)
        if (adrV.isSuccess) {
          if (pha.isDefined) {
            val adrOp = adrV.toOption.get
            if (adrOp.isDefined) {
              Success(Some((adrOp.get, pha.get.usage, pha.get.privacy)))
            } else {
              Failure(new RuntimeException("Address with ID " + aid + " not found."))
            }
          } else {
            Success(None)
          }
        } else {
          throw adrV.toEither.left.get
        }
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }

  /**
   * Get the [[List]] of [[Address]]es a [[Person]] has.
   *
   * Note, that the output list is sorted according to the position parameter in the relation table.
   *
   * @param p The [[Person]] to get the [[Address]]es for.
   * @return A [[Validation]] of either a [[Throwable]] or a [[List]] of [[Address]]es.
   */
  def getPersonAddresses(p: Person): Validation[Throwable, List[Address]] = {
    require(p.id.isDefined)
    db withSession {
      try {
        val result = for {
          pha <- PersonHasAddresses.sortBy(x => (x.pid, x.pos)) if pha.pid === p.id.get
          a <- Addresses if pha.aid === a.id
        } yield a
        Success(result.list)
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }

  /**
   * Load the [[Address]] identified by the given <em>id</em>.
   *
   * @param id Identifier of the [[Address]].
   * @return A [[Validation]] of either a [[Throwable]] or an optional [[Address]].
   */
  def load(id: Long): Validation[Throwable, Option[Address]] = db withSession {
    try {
      Success(Query(Addresses).filter(_.id === id).firstOption)
    } catch {
      case t: Throwable => Failure(t)
    }
  }

  /**
   * Insert the specified Address item in the database and set it into a relation to the given Organization.
   *
   * If the Organization is not yet persisted it will be in a first step.
   */
  def saveOrgAddress(o: Organization, a: Address): Validation[Throwable, Address] = {

    var o1 = o
    if (!o.id.isDefined) {
      val result = Organization.saveOrUpdate(o)
      if (result.isSuccess) {
        o1 = result.toOption.get
      } else {
        return Failure(result.toEither.left.get)
      }
    }
    db withSession {
      try {
        val isUpdate = if (a.id.isDefined) true else false
        val result1 = Address.saveOrUpdate(a)
        if (result1.isSuccess) {
          val l = Query(OrgHasAddresses).where(_.oid === o1.id.get).list
          if (!isUpdate) {
            OrgHasAddresses.insert(OrgHasAddress(o1.id.get, result1.toOption.get.id.get, l.length + 1))
          }
          result1
        } else
          Failure(result1.toEither.left.get)
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }

  /**
   * Insert the specified Address item in the database and set it into a relation to the given Person.
   *
   * If the person is not yet persisted it will be in a first step.
   */
  def savePersonAddress(p: Person, t: (Address, UsageType, Privacy)): Validation[Throwable, Address] = {

    var p1 = p
    if (!p.id.isDefined) {
      val result = Person.saveOrUpdate(p)
      if (result.isSuccess) {
        p1 = Person.saveOrUpdate(p).toOption.get
      } else {
        return Failure(result.toEither.left.get)
      }
    }
    db withSession {
      try {
        val isUpdate = if (t._1.id.isDefined) true else false
        val result1 = Address.saveOrUpdate(t._1)
        if (result1.isSuccess) {
          val l = Query(PersonHasAddresses).where(_.pid === p1.id.get).list
          if (!isUpdate) {
            PersonHasAddresses.insert(PersonHasAddress(p1.id.get, result1.toOption.get.id.get, l.length + 1, t._2, t._3))
          }
          result1
        } else
          Failure(result1.toEither.left.get)
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }

  /**
   * Persist a new [[Address]] instance or update an existing one.
   */
  def saveOrUpdate(a: Address): Validation[Throwable, Address] = {
    db withSession {

      require(Option(a).isDefined)
      // if the object has an identifier it is an update
      if (a.id.isDefined) {
        try {
          val count = Addresses.update(a)
          if (count == 0) {
            Failure(new RuntimeException("Failed to update address " + a))
          } else {
            Success(a)
          }
        } catch {
          case e: Throwable => Failure(e)
        }
      } // objects without identifier are new and must be inserted
      else {
        try {
          val id = Addresses.insert(a)
          Success(a.copy(id = Some(id)))
        } catch {
          case e: Throwable => Failure(e)
        }
      }
    }
  }

  /**
   * Delete the address identified by id.
   */
  def deleteOrgAddress(oid: Long, id: Long): Validation[Throwable, Boolean] = db withSession {
    val del = Query(OrgHasAddresses).where(_.oid === oid).where(_.aid === id).delete
    if (del > 0) {
      try {
        val delCount = Query(Addresses).filter(_.id === id).delete
        if (delCount > 0) Success(true) else Failure(new RuntimeException("Failed to delete address with id " + id))
      } catch {
        case e: Throwable => Failure(e)
      }
    } else {
      Failure(new RuntimeException("Failed to delete the connection between the organization and the address. (" + oid + ", " + id + ")"))
    }
  }

  /**
   * Count the number of occurrences of addresses.
   */
  def count(): Int = db withSession {
    Addresses.count()
  }
}

object Addresses extends Table[Address](Address.tablename) {

  import scala.slick.lifted.MappedTypeMapper.base
  import scala.slick.lifted.TypeMapper

  implicit val countryMapper: TypeMapper[Country] = base[Country, Int](c => c.id.get, id => Country.load(id).get)

  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

  def addon = column[String]("addon", O.Nullable)

  def street = column[String]("street", O.Nullable)

  def postbox = column[String]("postbox", O.Nullable)

  def city = column[String]("city")

  def zip = column[String]("zip")

  def country = column[Country]("country")

  def created = column[Long]("created")

  def creator = column[String]("creator")

  def modified = column[Long]("modified", O.Nullable)

  def modifier = column[String]("modifier", O.Nullable)

  def * = id.? ~ addon.? ~ street.? ~ postbox.? ~ city ~ zip ~ country ~ created ~ creator ~ modified.? ~ modifier.? <>(Address.apply _, Address.unapply _)

  def withoutId = addon.? ~ street.? ~ postbox.? ~ city ~ zip ~ country ~ created ~ creator ~ modified.? ~ modifier.? returning id

  def insert = (a: Address) => withoutId.insert(a.addon, a.street, a.postbox, a.city, a.zip, a.country, a.created, a.creator, a.modified, a.modifier)

  def update(a: Address): Int = Addresses.where(_.id === a.id).update(a.copy(modified = Some(System.currentTimeMillis())))

  def count(): Int = Addresses.count()
}
