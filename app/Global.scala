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
import models.PersonHasEmail
import models.Events
import models.Organizations
import models.Countries
import models.Addresses
import models.PersonHasAddress
import models.OrgHasAddress
import models.NewsTable

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
    	  PersonHasEmail.ddl.create
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
    	  PersonHasAddress.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }
      
      try {
    	  OrgHasAddress.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }
      
      try {
    	  NewsTable.ddl.create
      } catch {
        case e: PSQLException => Logger.logger.warn("IGNORING " + e.getMessage())
      }
    }

  }
}