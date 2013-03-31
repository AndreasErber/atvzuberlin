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
import models.Persons
import org.postgresql.util.PSQLException
import models.Emails
import models.Users
import models.Events
import models.Organizations
import models.Countries
import models.Addresses
import models.NewsTable
import models.Phones
import models.PersonHasAddresses
import models.OrgHasAddresses
import models.PersonHasPhones
import models.OrgHasPhones
import models.PersonHasEmails

/**
 * @author andreas
 * @version 0.0.7, 2013-03-30
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
      
    }
  }
}