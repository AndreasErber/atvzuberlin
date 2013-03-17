/**
 *
 */
package controllers

import play.api.data.Form
import play.api.data.Forms._
import play.api.Logger
import play.api.mvc._
import util.CustomFormatters
import util.UsageType
import models.Email
import play.api.Play.current
import models.Person
import models.PersonHasEmail
import controllers.ext.ProvidesCtx
import controllers.ext.Security
import play.api.i18n.Messages

/**
 * @author andreas
 * @version 0.0.2, 2013-03-13
 */
object EmailCtrl extends Controller with ProvidesCtx with Security {

  implicit val charFormatter = CustomFormatters.usageTypeFormatter
  val usageTypeMapping = of[UsageType]

  val emailForm = Form[Email](
    mapping(
      "id" -> optional(longNumber),
      "address" -> email,
      "usage" -> usageTypeMapping,
      "created" -> longNumber,
      "creator" -> text,
      "modified" -> optional(longNumber),
      "modifier" -> optional(text))(Email.apply)(Email.unapply))

  def createPersonEmail(pid: Long) = isAuthenticated { username => implicit request =>
    Ok(views.html.emailForm(emailForm.bind(Map("usage" -> "1")).discardingErrors, pid))
  }
  
  def deletePersonEmail(pid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>
      val result = Email.deletePersonEmail(pid, id)
      if (result.isSuccess) {
        Redirect(routes.EmailCtrl.showPersonEmail(pid)).flashing(("success" -> Messages("success.succeededToDeleteEmail")))
      } else {
        Logger.logger.error(result.toString(), result.fail.toOption.get)
        Redirect(routes.EmailCtrl.showPersonEmail(pid)).flashing(("error" -> Messages("error.failedToDeleteEmail")))
      }
  }

  def editPersonEmail(pid: Long, id: Long) = isAuthenticated { username => implicit request =>
    Ok(views.html.emailForm(emailForm.fill(Email.load(id).get), pid))
  }

  def showPersonEmail(pid: Long) = isAuthenticated { username => implicit request =>
    val p = Person.load(pid).get
    val req = Ok(views.html.email(p, "p", Email.getPersonEmails(p).toOption.get))
      if (flash.get("error").isDefined) {
        req.flashing(("error" -> flash.get("error").get))
      } else if (flash.get("success").isDefined) {
        req.flashing(("success" -> flash.get("success").get))
      } else {
        req
      }
  }
  def submitPersonEmail(pid: Long) = isAuthenticated { username => implicit request =>
    emailForm.bindFromRequest.fold(
      errors => {
        Logger.error("An error occurred when trying to process the email form.")
        BadRequest(views.html.emailForm(errors, pid))
      },
      email => {
        val p = Person.load(pid).get
        Logger.debug("Storing email address " + email.address + " for person " + p.lastname + ", " + p.firstname.getOrElse(""))
        val result = Email.savePersonEmail(p, email)
        if (result.isSuccess) {
          Redirect(routes.EmailCtrl.showPersonEmail(pid)).flashing(("success" -> Messages("success.succeededToStoreEmail")))
        } else {
          Logger.error(result.toString(),result.fail.toOption.get)
          BadRequest(views.html.emailForm(emailForm, pid)).flashing("error" -> Messages("error.failedToStoreEmail"))
        }
      })
  }
}