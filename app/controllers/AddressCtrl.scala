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
import models.{ Address, Addresses }
import play.api.Play.current
import models.Person
import models.PersonHasEmail
import controllers.ext.ProvidesCtx
import controllers.ext.Security
import models.Country
import play.api.i18n.Messages


/**
 * @author andreas
 * @version 0.0.2, 2013-03-13
 */
object AddressCtrl extends Controller with ProvidesCtx with Security {

  implicit val countryFormatter = CustomFormatters.countryFormatter
  val countryMapping = of[Country]

  val adrForm = Form[Address](
    mapping(
      "id" -> optional(longNumber),
      "addon" -> optional(text),
      "street" -> optional(text),
      "postbox" -> optional(text),
      "city" -> nonEmptyText,
      "zip" -> nonEmptyText,
      "country" -> countryMapping,
      "created" -> longNumber,
      "creator" -> text,
      "modified" -> optional(longNumber),
      "modifier" -> optional(text))(Address.apply)(Address.unapply))

  def createPersonAdr(pid: Long) = isAuthenticated { username =>
    implicit request =>
      val countries = Country.getAll
      if (countries.isSuccess) {
        Ok(views.html.adrForm(adrForm, countries.toOption.get, pid))
      } else {
        Ok(views.html.adrForm(adrForm.withGlobalError(Messages("error.failedToLoadCountries")), List(), pid))
      }
  }

  def deletePersonAdr(pid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>
      val result = Address.deletePersonAddress(pid, id)
      if (result.isSuccess) {
        Redirect(routes.AddressCtrl.showPersonAdr(pid)).flashing(("success" -> Messages("success.succeededToDeleteAddress")))
      } else {
        Logger.logger.error(result.toString(), result.fail.toOption.get)
        Redirect(routes.AddressCtrl.showPersonAdr(pid)).flashing(("error" -> Messages("error.failedToDeleteAddress")))
      }
  }

  def editPersonAdr(pid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>
      val countries = Country.getAll
      if (countries.isSuccess) {
        Ok(views.html.adrForm(adrForm.fill(Address.load(id).get), countries.toOption.get, pid))
      } else {
        Ok(views.html.adrForm(adrForm.fill(Address.load(id).get).withError("country", Messages("error.failedToLoadCountries")), List(), pid))
      }
  }

  def showPersonAdr(pid: Long) = isAuthenticated { username =>
    implicit request =>
      val p = Person.load(pid).get
      val req = Ok(views.html.adr(p, "p", Address.getPersonAddresses(p).toOption.get))
      if (flash.get("error").isDefined) {
        req.flashing(("error" -> flash.get("error").get))
      } else if (flash.get("success").isDefined) {
        req.flashing(("success" -> flash.get("success").get))
      } else {
        req
      }
  }

  def submitPersonAdr(pid: Long) = isAuthenticated { username =>
    implicit request =>
      adrForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the email form.")
          val countries = Country.getAll
          if (countries.isSuccess) {
            BadRequest(views.html.adrForm(errors, countries.toOption.get, pid))
          } else {
            BadRequest(views.html.adrForm(errors.withError("country", Messages("error.failedToLoadCountries")), List(), pid))
          }
        },
        adr => {
          val p = Person.load(pid).get
          Logger.debug("Storing address " + adr + " for person " + p.lastname + ", " + p.firstname)
          val result = Address.savePersonAddress(p, adr)
          if (result.isSuccess) {
            Redirect(routes.AddressCtrl.showPersonAdr(pid)).flashing(("success" -> Messages("success.succeededToStoreAddress")))
          } else {
            val formWithError = adrForm.withGlobalError(Messages("error.failedToStoreAddress"))
            Logger.error(result.toString(), result.fail.toOption.get)
            val countries = Country.getAll
            if (countries.isSuccess) {
              BadRequest(views.html.adrForm(formWithError, countries.toOption.get, pid))
            } else {
              BadRequest(views.html.adrForm(formWithError.withError("country", Messages("error.failedToLoadCountries")), List(), pid))
            }
          }
        })
  }
}