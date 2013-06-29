/**
 *
 */
package controllers

import models.Email
import models.Person
import models.User
import play.api._
import play.api.data._
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import play.api.i18n.Messages
import play.api.mvc.Controller
import play.api.mvc.Action
import play.api.mvc.Security
import play.api.Logger
import util.CustomFormatters
import util.UserRole
import util.StandardUser
import controllers.ext.ProvidesCtx
import util.Ctx
import controllers.ext.Security
import play.api.mvc.Flash

/**
 * @author andreas
 * @version 0.0.4, 2013-03-31
 */
object UserCtrl extends Controller with ProvidesCtx with Security {

  implicit val personFormatter = CustomFormatters.personFormatter
  implicit val emailFormatter = CustomFormatters.emailFormatter

  val emailMapping = of[Email]
  val personMapping = of[Person]

  val registrationForm = Form(
    tuple(
      "username" -> personMapping,
      "password1" -> nonEmptyText,
      "password2" -> nonEmptyText)
      .verifying(Messages("registration.error.password.too.short"), t => (t._2.length() > 8))
      .verifying(Messages("registration.error.password.confirmation.failed"), t => t._2 == t._3))

  def displayLoginForm() = Action { implicit request =>
    Logger.logger.debug("Display login form")
    val req = Ok(views.html.login(Ctx.loginForm, None))
    if (flash.get("error").isDefined) {
      req.flashing("error" -> flash.get("error").get)
    } else if (flash.get("success").isDefined) {
      req.flashing("success" -> flash.get("success").get)
    } else {
      req
    }
  }

  def index() = Action { implicit request =>
    Ok(views.html.index("Des is die Messetsch!"))

  }

  /**
   * Trying to log in a user.
   */
  def login() = Action { implicit request =>
    Logger.logger.debug("Authenticating user ...")
    // binding will evaluate the credentials against the DB
    Ctx.loginForm.bindFromRequest.fold(
      hasErrors = { form =>
        Logger.logger.error("Authentication of user '" + form.data.get("username").get + "' failed.")
        Redirect(routes.UserCtrl.index).withNewSession.flashing(Flash(form.data) + ("error" -> Messages("error.loginFailed")))
      },
      tuple => {
        val result = User.auth(tuple._1, tuple._2)
        if (result.isSuccess) {
          Logger.logger.debug("Authentication of user " + tuple._1 + " was successful!")
          Redirect(getCtxt.referer.get).withSession(Security.username -> tuple._1).flashing("success" -> Messages("login.successful"))
        } else {
          Logger.logger.debug("Authentication of user " + tuple._1 + " failed!", result.fail.toOption.get)
          Redirect(routes.UserCtrl.displayLoginForm).flashing("error" -> Messages("error.loginFailed"))
        }
      })
  }

  def logout = Action { implicit request =>
    Redirect(getCtxt.referer.get).withNewSession.flashing("success" -> Messages("logout.successful"))
  }

  def showRegistrationForm = Action { implicit request =>
    Logger.logger.debug("Display registration form")
    val req = Ok(views.html.register(registrationForm, getUsernameSelection))
    if (flash.get("error").isDefined) {
      req.flashing("error" -> flash.get("error").get)
    } else if (flash.get("success").isDefined) {
      req.flashing("success" -> flash.get("success").get)
    } else {
      req
    }
  }

  def register = Action { implicit request =>
    Logger.logger.debug("Registering user ...")
    // binding will evaluate the credentials against the DB
    registrationForm.bindFromRequest.fold(
      errors => {
        Logger.logger.error("Registration failed.")
        val req = BadRequest(views.html.register(errors, getUsernameSelection))
        if (errors.globalError.isDefined) {
          Logger.logger.error("Global error: " + errors.globalError.get.message)
          req.flashing(("error" -> errors.globalError.get.message))
        } else {
          req
        }
      },
      tuple => {
        var un = ""
        if (!tuple._1.nickname.isDefined) {
          un = tuple._1.firstname.getOrElse("")
          if (un.length > 0) {
            un += "."
          }
          un += tuple._1.lastname
        } else {
          un = tuple._1.nickname.get
        }
        val u = User(un, tuple._2, Email.getPersonEmails(tuple._1).toOption.get(0)._1, tuple._1.id.get, StandardUser, System.currentTimeMillis(), None)
        val result = User.insert(u)
        if (result.isSuccess) {
          Logger.logger.debug("Registration of user " + u.username + " was successful!")
          Redirect(routes.UserCtrl.index).flashing("success" -> Messages("success.registrationSuccessful"))
        } else {
          Logger.logger.error("Registration of user " + u.username + " failed!", result.fail.toOption.get)
          BadRequest(views.html.register(registrationForm, getUsernameSelection)).flashing("error" -> Messages("error.registrationFailed"))
        }
      })
  }

  private def getUsernameSelection = {
    val list = Person.getAll
    if (list.isSuccess) {
      for (p <- list.toOption.get) yield (p.id.get.toString, p.lastname + " - " + p.nickname.getOrElse(""))
    } else {
      Nil
    }
  }
}