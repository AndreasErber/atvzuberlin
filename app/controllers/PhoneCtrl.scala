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
import models.Organization

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

  val phoneMapping = mapping(
    "id" -> optional(longNumber),
    "areacode" -> number,
    "extension" -> number,
    "country" -> countryMapping,
    "kind" -> phoneTypeMapping,
    "created" -> longNumber,
    "creator" -> text,
    "modified" -> optional(longNumber),
    "modifier" -> optional(text))(Phone.apply)(Phone.unapply)

  val phoneForm = Form[(Phone, UsageType, Privacy)](
    tuple(
      "phone" -> phoneMapping,
      "usage" -> usageTypeMapping,
      "privacy" -> privacyMapping))

  val phoneOrgForm = Form[Phone](
    phoneMapping)

  def createOrgPhone(oid: Long) = isAuthenticated { username =>
    implicit request =>
      val countries = Country.getAll
      if (countries.isSuccess) {
        Ok(views.html.phoneOrgForm(phoneOrgForm, countries.toOption.get, oid))
      } else {
        Ok(views.html.phoneOrgForm(phoneOrgForm.withGlobalError(Messages("error.failedToLoadCountries")), List(), oid))
      }
  }
    
  def createPersonPhone(pid: Long) = isAuthenticated { username =>
    implicit request =>
      val countries = Country.getAll
      if (countries.isSuccess) {
        Ok(views.html.phoneForm(phoneForm.bind(Map("kind" -> "1", "usage" -> "1", "privacy" -> "2")).discardingErrors, countries.toOption.get, pid))
      } else {
        Ok(views.html.phoneForm(phoneForm.withGlobalError(Messages("error.failedToLoadCountries")), List(), pid))
      }
  }

  def deleteOrgPhone(oid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>
      val result = Phone.deleteOrgPhone(oid, id)
      if (result.isSuccess) {
        Redirect(routes.PhoneCtrl.showOrgPhone(oid)).flashing(("success" -> Messages("success.succeededToDeletePhone")))
      } else {
        Logger.logger.error(result.toString(), result.fail.toOption.get)
        Redirect(routes.PhoneCtrl.showOrgPhone(oid)).flashing(("error" -> Messages("error.failedToDeletePhone")))
      }
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
  
  def editOrgPhone(oid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>
      val result = Phone.getOrgPhone(Organization.load(oid).get, id)
      if (result.isSuccess) {
        val countries = Country.getAll
        if (countries.isSuccess) {
          Ok(views.html.phoneOrgForm(phoneOrgForm.fill(result.toOption.get.get), countries.toOption.get, oid))
        } else {
          Ok(views.html.phoneOrgForm(phoneOrgForm.fill(result.toOption.get.get).withError("country", Messages("error.failedToLoadCountries")), List(), oid))
        }
      } else {
        Logger.error(result.toString(), result.fail.toOption.get)
        Redirect(routes.PhoneCtrl.showOrgPhone(oid)).flashing(("error" -> Messages("error.failedToLoadAddress")))
      }
  }

  def editPersonPhone(pid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>
      val result = Phone.getPersonPhone(Person.load(pid).get, id)
      if (result.isSuccess) {
        val countries = Country.getAll
        if (countries.isSuccess) {
          Ok(views.html.phoneForm(phoneForm.fill(result.toOption.get.get), countries.toOption.get, pid))
        } else {
          Ok(views.html.phoneForm(phoneForm.fill(result.toOption.get.get).withError("country", Messages("error.failedToLoadCountries")), List(), pid))
        }
      } else {
        Logger.error(result.toString(), result.fail.toOption.get)
        Redirect(routes.PhoneCtrl.showPersonPhone(pid)).flashing(("error" -> Messages("error.failedToLoadAddress")))
      }
  }

  def showOrgPhone(oid: Long) = isAuthenticated { username =>
    implicit request =>
      val o = Organization.load(oid).get
      val req = Ok(views.html.phoneOrg(o, Phone.getOrgPhones(o).toOption.get))
      if (flash.get("error").isDefined) {
        req.flashing(("error" -> flash.get("error").get))
      } else if (flash.get("success").isDefined) {
        req.flashing(("success" -> flash.get("success").get))
      } else {
        req
      }
  }
    
  def showPersonPhone(pid: Long) = isAuthenticated { username =>
    implicit request =>
      val p = Person.load(pid).get
      val req = Ok(views.html.phone(p, Phone.getPersonPhones(p).toOption.get))
      if (flash.get("error").isDefined) {
        req.flashing(("error" -> flash.get("error").get))
      } else if (flash.get("success").isDefined) {
        req.flashing(("success" -> flash.get("success").get))
      } else {
        req
      }
  }
  
  def submitOrgPhone(oid: Long) = isAuthenticated { username =>
    implicit request =>
      phoneOrgForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the phone org form.")
          for (err <- errors.errors) {
            Logger.error(err.key + " - " + err.message)
          }
          for (datum <- errors.data) {
            Logger.debug(datum._1 + " - " + datum._2)
          }
          val countries = Country.getAll
          if (countries.isSuccess) {
            BadRequest(views.html.phoneOrgForm(errors, countries.toOption.get, oid))
          } else {
            BadRequest(views.html.phoneOrgForm(errors.withError("country", Messages("error.failedToLoadCountries")), List(), oid))
          }
        },
        ph => {
          val o = Organization.load(oid).get
          Logger.debug("Storing phone number " + ph.country.phone + " " + ph.areacode + " " + ph.extension + " for organization " + o.name)
          val result = Phone.saveOrgPhone(o, ph)
          if (result.isSuccess) {
            Redirect(routes.PhoneCtrl.showOrgPhone(oid)).flashing(("success" -> Messages("success.succeededToStorePhone")))
          } else {
            val formWithError = phoneOrgForm.withGlobalError(Messages("error.failedToStorePhone"))
            val countries = Country.getAll
            if (countries.isSuccess) {
              Logger.error(result.toString(), result.fail.toOption.get)
              BadRequest(views.html.phoneOrgForm(formWithError, countries.toOption.get, oid))
            } else {
              BadRequest(views.html.phoneOrgForm(formWithError.withError("country", Messages("error.failedToLoadCountries")), List(), oid))
            }
          }
        })
  }

  def submitPersonPhone(pid: Long) = isAuthenticated { username =>
    implicit request =>
      phoneForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the phone form.")
          for (err <- errors.errors) {
            Logger.error(err.key + " - " + err.message)
          }
          for (datum <- errors.data) {
            Logger.debug(datum._1 + " - " + datum._2)
          }
          val countries = Country.getAll
          if (countries.isSuccess) {
            BadRequest(views.html.phoneForm(errors, countries.toOption.get, pid))
          } else {
            BadRequest(views.html.phoneForm(errors.withError("country", Messages("error.failedToLoadCountries")), List(), pid))
          }
        },
        ph => {
          val p = Person.load(pid).get
          Logger.debug("Storing phone number " + ph._1.country.phone + " " + ph._1.areacode + " " + ph._1.extension + " for person " + p.lastname + ", " + p.firstname.getOrElse(""))
          val result = Phone.savePersonPhone(p, ph._1)
          if (result.isSuccess) {
            Redirect(routes.PhoneCtrl.showPersonPhone(pid)).flashing(("success" -> Messages("success.succeededToStorePhone")))
          } else {
            val formWithError = phoneForm.withGlobalError(Messages("error.failedToStorePhone"))
            val countries = Country.getAll
            if (countries.isSuccess) {
              Logger.error(result.toString(), result.fail.toOption.get)
              BadRequest(views.html.phoneForm(formWithError, countries.toOption.get, pid))
            } else {
              BadRequest(views.html.phoneForm(formWithError.withError("country", Messages("error.failedToLoadCountries")), List(), pid))
            }
          }
        })
  }
}