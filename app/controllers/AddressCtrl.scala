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
import util.Privacy
import models.Organization

/**
 * @author andreas
 * @version 0.0.3, 2013-03-19
 */
object AddressCtrl extends Controller with ProvidesCtx with Security {

  implicit val countryFormatter = CustomFormatters.countryFormatter
  val countryMapping = of[Country]

  implicit val usageFormatter = CustomFormatters.usageTypeFormatter
  val usageMapping = of[UsageType]

  implicit val privacyFormatter = CustomFormatters.privacyFormatter
  val privacyMapping = of[Privacy]

  val adrMapping = mapping(
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
    "modifier" -> optional(text))(Address.apply)(Address.unapply)

  val adrPersForm = Form[(Address, UsageType, Privacy)](
    tuple(
      "address" -> adrMapping,
      "usage" -> usageMapping,
      "privacy" -> privacyMapping))

  val adrOrgForm = Form[Address](adrMapping)

  def createPersonAdr(pid: Long) = isAuthenticated { username =>
    implicit request =>
      val countries = Country.getAll
      if (countries.isSuccess) {
        Ok(views.html.adrForm(adrPersForm.bind(Map("usage" -> "1", "privacy" -> "2")).discardingErrors, countries.toOption.get, pid))
      } else {
        Ok(views.html.adrForm(adrPersForm.withGlobalError(Messages("error.failedToLoadCountries")), List(), pid))
      }
  }

  def createOrgAdr(oid: Long) = isAuthenticated { username =>
    implicit request =>
      val countries = Country.getAll
      if (countries.isSuccess) {
        Ok(views.html.adrOrgForm(adrOrgForm.bind(Map("usage" -> "1", "privacy" -> "2")).discardingErrors, countries.toOption.get, oid))
      } else {
        Ok(views.html.adrOrgForm(adrOrgForm.withGlobalError(Messages("error.failedToLoadCountries")), List(), oid))
      }
  }

  def deleteOrgAdr(oid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>
      val result = Address.deleteOrgAddress(id, id)
      if (result.isSuccess) {
        Redirect(routes.AddressCtrl.showOrgAdr(oid)).flashing(("success" -> Messages("success.succeededToDeleteAddress")))
      } else {
        Logger.error(result.toString(), result.fail.toOption.get)
        Redirect(routes.AddressCtrl.showOrgAdr(oid)).flashing(("error" -> Messages("error.failedToDeleteAddress")))
      }
  }

  def deletePersonAdr(pid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>
      val result = Address.deletePersonAddress(pid, id)
      if (result.isSuccess) {
        Redirect(routes.AddressCtrl.showPersonAdr(pid)).flashing(("success" -> Messages("success.succeededToDeleteAddress")))
      } else {
        Logger.error(result.toString(), result.fail.toOption.get)
        Redirect(routes.AddressCtrl.showPersonAdr(pid)).flashing(("error" -> Messages("error.failedToDeleteAddress")))
      }
  }

  def editOrgAdr(oid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>

      val result = Address.getOrgAddress(Organization.load(oid).get, id)
      if (result.isSuccess) {
        val countries = Country.getAll
        if (countries.isSuccess) {
          Ok(views.html.adrOrgForm(adrOrgForm.fill(result.toOption.get.get), countries.toOption.get, oid))
        } else {
          Ok(views.html.adrOrgForm(adrOrgForm.fill(result.toOption.get.get).withError("country", Messages("error.failedToLoadCountries")), List(), oid))
        }
      } else {
        Logger.error(result.toString(), result.fail.toOption.get)
        Redirect(routes.AddressCtrl.showOrgAdr(oid)).flashing(("error" -> Messages("error.failedToLoadAddress")))
      }
  }

  def editPersonAdr(pid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>

      val result = Address.getPersonAddress(Person.load(pid).get, id)
      if (result.isSuccess) {
        val countries = Country.getAll
        if (countries.isSuccess) {
          Ok(views.html.adrForm(adrPersForm.fill(result.toOption.get.get), countries.toOption.get, pid))
        } else {
          Ok(views.html.adrForm(adrPersForm.fill(result.toOption.get.get).withError("country", Messages("error.failedToLoadCountries")), List(), pid))
        }
      } else {
        Logger.error(result.toString(), result.fail.toOption.get)
        Redirect(routes.AddressCtrl.showPersonAdr(pid)).flashing(("error" -> Messages("error.failedToLoadAddress")))
      }
  }

  def showOrgAdr(oid: Long) = isAuthenticated { username =>
    implicit request =>
      val o = Organization.load(oid).get
      val req = Ok(views.html.adr(o, "o", Address.getOrgAddresses(o).toOption.get))
      if (flash.get("error").isDefined) {
        req.flashing(("error" -> flash.get("error").get))
      } else if (flash.get("success").isDefined) {
        req.flashing(("success" -> flash.get("success").get))
      } else {
        req
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

  def submitOrgAdr(oid: Long) = isAuthenticated { username =>
    implicit request =>
      adrOrgForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process organization address form.")
          val countries = Country.getAll
          if (countries.isSuccess) {
            BadRequest(views.html.adrOrgForm(errors, countries.toOption.get, oid))
          } else {
            BadRequest(views.html.adrOrgForm(errors.withError("country", Messages("error.failedToLoadCountries")), List(), oid))
          }
        },
        adr => {
          val o = Organization.load(oid).get
          Logger.debug("Storing address " + adr + " for organization " + o.name)
          val result = Address.saveOrgAddress(o, adr)
          if (result.isSuccess) {
            Redirect(routes.AddressCtrl.showOrgAdr(oid)).flashing(("success" -> Messages("success.succeededToStoreAddress")))
          } else {
            val formWithError = adrOrgForm.withGlobalError(Messages("error.failedToStoreAddress"))
            Logger.error(result.toString(), result.fail.toOption.get)
            val countries = Country.getAll
            if (countries.isSuccess) {
              BadRequest(views.html.adrOrgForm(formWithError, countries.toOption.get, oid))
            } else {
              BadRequest(views.html.adrOrgForm(formWithError.withError("country", Messages("error.failedToLoadCountries")), List(), oid))
            }
          }
        })
  }

  def submitPersonAdr(pid: Long) = isAuthenticated { username =>
    implicit request =>
      adrPersForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process person address form.")
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
            val formWithError = adrPersForm.withGlobalError(Messages("error.failedToStoreAddress"))
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