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
import util.Privacy
import models.Organization

/**
 * @author andreas
 * @version 0.0.5, 2015-01-03
 */
object EmailCtrl extends Controller with ProvidesCtx with Security {

  implicit val charFormatter = CustomFormatters.usageTypeFormatter
  val usageTypeMapping = of[UsageType]

  implicit val privacyFormatter = CustomFormatters.privacyFormatter
  val privacyMapping = of[Privacy]

  val emailMapping = mapping(
    "id" -> optional(longNumber),
    "address" -> email,
    "created" -> longNumber,
    "creator" -> text,
    "modified" -> optional(longNumber),
    "modifier" -> optional(text))(Email.apply)(Email.unapply)

  val emailForm = Form[(Email, UsageType, Privacy)](
    tuple("email" -> emailMapping,
      "usage" -> usageTypeMapping,
      "privacy" -> privacyMapping))

  val emailOrgForm = Form[Email](emailMapping)

  def createOrgEmail(oid: Long) = isAuthenticated { username =>
    implicit request =>
      Ok(views.html.emailOrgForm(emailOrgForm, oid))
  }

  def createPersonEmail(pid: Long) = isAuthenticated { username =>
    implicit request =>
      Ok(views.html.emailForm(emailForm.bind(Map("usage" -> "1", "privacy" -> "2")).discardingErrors, pid))
  }

  def deleteOrgEmail(oid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>
      val result = Email.deleteOrgEmail(oid, id)
      if (result.isSuccess) {
        Redirect(routes.EmailCtrl.showOrgEmail(oid)).flashing(("success" -> Messages("success.succeededToDeleteEmail")))
      } else {
        Logger.logger.error(result.toString(), result.toEither.left.get)
        Redirect(routes.EmailCtrl.showOrgEmail(oid)).flashing(("error" -> Messages("error.failedToDeleteEmail")))
      }
  }

  def deletePersonEmail(pid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>
      val result = Email.deletePersonEmail(pid, id)
      if (result.isSuccess) {
        Redirect(routes.EmailCtrl.showPersonEmail(pid)).flashing(("success" -> Messages("success.succeededToDeleteEmail")))
      } else {
        Logger.logger.error(result.toString(), result.toEither.left.get)
        Redirect(routes.EmailCtrl.showPersonEmail(pid)).flashing(("error" -> Messages("error.failedToDeleteEmail")))
      }
  }

  def editOrgEmail(oid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>
      Ok(views.html.emailOrgForm(emailOrgForm.fill(Email.load(id).get), oid))
  }

  def editPersonEmail(pid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>
      val result = Email.getPersonEmail(Person.load(pid).get, id)
      if (result.isSuccess) {
        val x = result.toOption.get
        if (x.isDefined) {
          Ok(views.html.emailForm(emailForm.fill((x.get._1, x.get._2.usage, x.get._2.privacy)), pid))
        } else {
          Redirect(routes.EmailCtrl.showPersonEmail(pid)).flashing("error" -> Messages("error.failedToLoadEmail"))
        }
      } else {
        Redirect(routes.EmailCtrl.showPersonEmail(pid)).flashing("error" -> Messages("error.failedToLoadEmail"))
      }
  }

  def showOrgEmail(oid: Long) = isAuthenticated { username =>
    implicit request =>
      val o = Organization.load(oid).get
      val req = Ok(views.html.emailOrg(o, Email.getOrgEmails(o).toOption.get))
      if (request.flash.get("error").isDefined) {
        req.flashing(("error" -> request.flash.get("error").get))
      } else if (request.flash.get("success").isDefined) {
        req.flashing(("success" -> request.flash.get("success").get))
      } else {
        req
      }
  }

  def showPersonEmail(pid: Long) = isAuthenticated { username =>
    implicit request =>
      val p = Person.load(pid).get
      val req = Ok(views.html.email(p, Email.getPersonEmails(p).toOption.get))
      if (request.flash.get("error").isDefined) {
        req.flashing(("error" -> request.flash.get("error").get))
      } else if (request.flash.get("success").isDefined) {
        req.flashing(("success" -> request.flash.get("success").get))
      } else {
        req
      }
  }

  def submitOrgEmail(oid: Long) = isAuthenticated { username =>
    implicit request =>
      Logger.debug("Binding org email request ...")
      emailOrgForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the organization email form. Hint: " + errors.error("address").toString())
          BadRequest(views.html.emailOrgForm(errors, oid))
        },
        email => {
          val o = Organization.load(oid).get
          Logger.debug("Storing email address " + email.address + " for organization " + o.name)
          val result = Email.saveOrgEmail(o, email)
          if (result.isSuccess) {
            Redirect(routes.EmailCtrl.showOrgEmail(oid)).flashing(("success" -> Messages("success.succeededToStoreEmail")))
          } else {
            Logger.error(result.toString(), result.toEither.left.get)
            BadRequest(views.html.emailOrgForm(emailOrgForm, oid)).flashing("error" -> Messages("error.failedToStoreEmail"))
          }
        })
  }

  def submitPersonEmail(pid: Long) = isAuthenticated { username =>
    implicit request =>
      emailForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the email form.")
          BadRequest(views.html.emailForm(errors, pid))
        },
        email => {
          val p = Person.load(pid).get
          Logger.debug("Storing email address " + email._1.address + " for person " + p.lastname + ", " + p.firstname.getOrElse(""))
          val result = Email.savePersonEmail(p, email._1, email._2, email._3)
          if (result.isSuccess) {
            Redirect(routes.EmailCtrl.showPersonEmail(pid)).flashing(("success" -> Messages("success.succeededToStoreEmail")))
          } else {
            Logger.error(result.toString(), result.toEither.left.get)
            BadRequest(views.html.emailForm(emailForm, pid)).flashing("error" -> Messages("error.failedToStoreEmail"))
          }
        })
  }
}