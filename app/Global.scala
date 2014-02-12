/**
 *
 */
import play.api.db.DB
import play.api.GlobalSettings
import scala.slick.driver.PostgresDriver.simple._
import Database.threadLocalSession
import play.api.Application
import play.api.Logger
import play.api.Play.current
import models._
import org.postgresql.util.PSQLException
import play.api._
import play.api.mvc._
import play.api.mvc.Results._
import play.api.i18n.Messages
import accesscontrol.Roles
import accesscontrol.Privileges
import accesscontrol.RoleHasPrivileges

/**
 * @author andreas
 * @version 0.0.11, 2013-07-14
 */
object Global extends GlobalSettings {

  override def onStart(app: Application) {

    lazy val database = Database.forDataSource(DB.getDataSource())

    database.withSession {
      try {
        Persons.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }

      try {
        Emails.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }

      try {
        PersonHasEmails.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }

      try {
        Users.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }

      try {
        Events.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }

      try {
        Organizations.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }

      try {
        Countries.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }

      try {
        Addresses.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }

      try {
        PersonHasAddresses.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }

      try {
        OrgHasAddresses.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }

      try {
        NewsTable.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }

      try {
        Phones.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }

      try {
        PersonHasPhones.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }

      try {
        OrgHasPhones.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }

      try {
        OrgHasEmails.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }

      try {
        Homepages.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }

      try {
        OrgHasHomepages.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }

      try {
        PersonHasHomepages.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }

      try {
        PersonAdditionalInfos.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }

      try {
        AcademicTitles.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }

      try {
        PersonHasTitles.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }

      try {
        Enrollments.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }

      try {
        Sportss.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }

      try {
        SportsDates.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }

      try {
        Charges.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }

      try {
        PersonInCharges.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }

      try {
        Roles.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }

      try {
        Privileges.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }

      try {
        RoleHasPrivileges.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }
    }
  }

  override def onError(request: RequestHeader, ex: Throwable) = {
    Ok("/").flashing("error" -> ex.getMessage())
  }
}