package controllers

import models.{ Person, Persons }
import util.CustomFormatters
import play.api.data.Form
import play.api.data.Forms._
import play.api.db._
import play.api.Logger
import play.api.mvc._
import play.api.mvc.Security._
import play.api.Play.current
import models.Email
import controllers.ext.ProvidesCtx
import controllers.ext.Security
import models.Address

/**
 * @author andreas
 * @version 0.2.0, 2013-03-13
 */
object PersonCtrl extends Controller with ProvidesCtx with Security {

  implicit val charFormatter = CustomFormatters.charFormatter
  implicit val sqlDateFormatter = CustomFormatters.sqlDateFormatter

  val charMapping = of[Char]
  val sqlDateMapping = of[java.sql.Date]

  val personForm = Form(
    mapping(
      "id" -> optional(longNumber),
      "lastname" -> text,
      "firstname" -> optional(text),
      "nickname" -> optional(text),
      "birth" -> optional(sqlDateMapping),
      "death" -> optional(sqlDateMapping),
      "gender" -> charMapping,
      "created" -> longNumber,
      "creator" -> text,
      "modified" -> optional(longNumber),
      "modifier" -> optional(text))
      (Person.apply)(Person.unapply))

  /**
   * Display the form to create a new person.
   */
  def create = isAuthenticated { username =>
    implicit request =>
      Ok(views.html.personForm(personForm))
  }

  def delete(id: Long) = isAuthenticated { username =>
    implicit request =>
      val result = Person.delete(id)
      if (result.isSuccess) {
        Logger.logger.debug("Sucessfully deleted person with ID " + id + ".")
        Redirect(routes.PersonCtrl.list)
      } else {
        Logger.logger.error(result.toString(), result.fail.toOption.get)
        Redirect(routes.PersonCtrl.show(id))
      }
  }

  /**
   * Provide a list of all available person items.
   */
  def list = isAuthenticated { username =>
    implicit request =>
      val list = Person.getAll
      if (list.isSuccess) {
        Ok(views.html.personList(list.toOption.get.sortBy(x => (x.lastname, x.firstname))))
      } else {
        BadRequest("When trying to load the list of persons a failure occurred.")
      }
  }

  /**
   * Display the details of a person.
   */
  def show(id: Long) = isAuthenticated { username =>
    implicit request =>
      val p = Person.load(id)
      p match {
        case None =>
          Logger.logger.debug("No person with ID " + id + " found."); NotFound
        case Some(pers) =>
          Logger.logger.debug("Found person with ID " + id + ".")
          val alist = Address.getPersonAddresses(pers)
          val adrs = if (alist.isSuccess) alist.toOption.get else Nil
          val elist = Email.getPersonEmails(pers)
          val emails = if (elist.isSuccess) elist.toOption.get else Nil
          Ok(views.html.person(pers, adrs, emails))
        case _ => NotFound
      }
  }

  def edit(id: Long) = isAuthenticated { username =>
    implicit request =>
      val p = Person.load(id)
      p match {
        case None =>
          Logger.logger.debug("Cannot find person with ID " + id + "."); NotFound
        case Some(pers) =>
          Logger.logger.debug("Preparing editing of person with ID " + id + ".");
          Ok(views.html.personForm(personForm.fill(pers)))
        case _ => NotFound
      }
  }
  
  def submit = isAuthenticated { username =>
    implicit request =>
      personForm.bindFromRequest.value map {
        p =>
          val result = Person.saveOrUpdate(p)
          if (result.isSuccess) {
            Redirect(routes.PersonCtrl.show(result.toOption.get.id.get))
          } else {
            Logger.logger.error(result.toString(), result.fail.toOption.get)
            Redirect(routes.PersonCtrl.list)
          }

      } getOrElse BadRequest
  }


  def updatePerson = isAuthenticated { username =>
    implicit request =>
      personForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the person form.")
          BadRequest(views.html.personForm(errors))
        },
        person => {
          Logger.debug("Storing person " + person.lastname + ", " + person.firstname.getOrElse(""))
          val result = Person.saveOrUpdate(person)
          if (result.isSuccess) {
            Redirect(routes.PersonCtrl.show(person.id.get))
          } else {
            Logger.error(result.toString(), result.fail.toOption.get)
            BadRequest(views.html.personForm(personForm.fill(person)))
          }
        })
  }
}