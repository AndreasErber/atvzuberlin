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
import models.Phone
import models.Country
import util.PhoneType

/**
 * @author andreas
 * @version 0.0.1, 2013-03-19
 */
object PhoneCtrl extends Controller with ProvidesCtx with Security {

  implicit val countryFormatter = CustomFormatters.countryFormatter
  val countryMapping = of[Country]
  
  implicit val phoneTypeFormatter = CustomFormatters.phoneFormatter
  val phoneTypeMapping = of[PhoneType]

  implicit val charFormatter = CustomFormatters.usageTypeFormatter
  val usageTypeMapping = of[UsageType]

  implicit val privacyFormatter = CustomFormatters.privacyFormatter
  val privacyMapping = of[Privacy]

  val phoneForm = Form[Phone](
    mapping(
      "id" -> optional(longNumber),
      "areacode" -> number,
      "extension" -> number,
      "country" -> countryMapping, 
      "kind" -> phoneTypeMapping,
      "usage" -> usageTypeMapping,
      "privacy" -> privacyMapping,
      "created" -> longNumber,
      "creator" -> text,
      "modified" -> optional(longNumber),
      "modifier" -> optional(text))(Phone.apply)(Phone.unapply))

  def createPersonPhone(pid: Long) = isAuthenticated { username => implicit request =>
    Ok(views.html.phoneForm(phoneForm.bind(Map("kind" -> "1", "usage" -> "1", "privacy" -> "2")).discardingErrors, pid))
  }
  
  def deletePersonPhone(pid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>
      val result = Phone.deletePersonPhone(pid, id)
      if (result.isSuccess) {
        Redirect(routes.PhoneCtrl.showPersonPhone(pid)).flashing(("success" -> Messages("success.succeededToDeletePhone")))
      } else {
        Logger.logger.error(result.toString(), result.fail.toOption.get)
        Redirect(routes.PhoneCtrl.showPersonPhone(pid)).flashing(("error" -> Messages("error.failedToDeletePhone")))
      }
  }

  def editPersonPhone(pid: Long, id: Long) = isAuthenticated { username => implicit request =>
    Ok(views.html.emailPhone(emailPhone.fill(Phone.load(id).get), pid))
  }

  def showPersonPhone(pid: Long) = isAuthenticated { username => implicit request =>
    val p = Person.load(pid).get
    val req = Ok(views.html.phone(p, "p", Phone.getPersonPhones(p).toOption.get))
      if (flash.get("error").isDefined) {
        req.flashing(("error" -> flash.get("error").get))
      } else if (flash.get("success").isDefined) {
        req.flashing(("success" -> flash.get("success").get))
      } else {
        req
      }
  }

  def submitPersonPhone(pid: Long) = isAuthenticated { username => implicit request =>
    phoneForm.bindFromRequest.fold(
      errors => {
        Logger.error("An error occurred when trying to process the phone form.")
        BadRequest(views.html.phoneForm(errors, pid))
      },
      ph => {
        val p = Person.load(pid).get
        Logger.debug("Storing phone number " + ph.country.phone + " " + ph.areacode + " " + ph.extension + " for person " + p.lastname + ", " + p.firstname.getOrElse(""))
        val result = Phone.savePersonPhone(p, email)
        if (result.isSuccess) {
          Redirect(routes.PhoneCtrl.showPersonPhone(pid)).flashing(("success" -> Messages("success.succeededToStorePhone")))
        } else {
          Logger.error(result.toString(),result.fail.toOption.get)
          BadRequest(views.html.phoneForm(phoneForm, pid)).flashing("error" -> Messages("error.failedToStorePhone"))
        }
      })
  }
}