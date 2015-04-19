/**
 *
 */
package controllers

import play.api.data.Form
import play.api.data.Forms._
import play.api.Logger
import play.api.mvc._
import util.{CustomFormatters, PhoneType, Privacy, UsageType}
import models.{Country, Organization, Person, Phone}
import controllers.ext.{ProvidesCtx, Security}
import play.api.i18n.Messages

import scalaz.{Failure, Success}

/**
 * Entity to represent phone information.
 *
 * @author andreas
 * @version 0.0.3, 2015-04-19
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
        Redirect(routes.PhoneCtrl.showOrgPhone(oid)).flashing("success" -> Messages("success.succeededToDeletePhone"))
      } else {
        Logger.logger.error(result.toString, result.toEither.left.get)
        Redirect(routes.PhoneCtrl.showOrgPhone(oid)).flashing("error" -> Messages("error.failedToDeletePhone"))
      }
  }

  def deletePersonPhone(pid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>
      Logger.debug(s"Request to delete association between phone with ID $id and person with ID $pid.")
      val result = Phone.deletePersonPhone(pid, id)
      if (result.isSuccess) {
        Redirect(routes.PhoneCtrl.showPersonPhone(pid)).flashing("success" -> Messages("success.succeededToDeletePhone"))
      } else {
        Logger.logger.error(result.toString, result.toEither.left.get)
        Redirect(routes.PhoneCtrl.showPersonPhone(pid)).flashing("error" -> Messages("error.failedToDeletePhone"))
      }
  }

  def editOrgPhone(oid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>
      val orgV = Organization.load(oid)
      orgV match {
        case Success(orgOp) => orgOp match {
          case Some(org) => val phoneV = Phone.getOrgPhone(org, id)
            phoneV match {
              case Success(phoneOp) => phoneOp match {
                case Some(phone) => val countriesV = Country.getAll
                  countriesV match {
                    case Success(countries) => Ok(views.html.phoneOrgForm(phoneOrgForm.fill(phone), countries, oid))
                    case Failure(t) => Logger.error(countriesV.toString, t)
                      Redirect(routes.OrganizationCtrl.show(oid)).flashing("error" -> Messages("error.loading" +
                        ".phone.countries"))
                  }
                case None => Logger.error(s"Failed to load phone with ID '$id'. Does not exist.")
                  Redirect(routes.OrganizationCtrl.show(oid)).flashing("error" -> Messages("error.loading.phone"))
              }
              case Failure(t) => Logger.error(phoneV.toString, t)
                Redirect(routes.OrganizationCtrl.show(oid)).flashing("error" -> Messages("error.loading.phone"))
            }
          case None => Logger.error(s"Failed to load organization with ID '$oid'. Does not exist.")
            Redirect(routes.OrganizationCtrl.list()).flashing("error" -> Messages("error.loading.organization"))
        }
        case Failure(t) => Logger.error(orgV.toString, t)
          Redirect(routes.OrganizationCtrl.list()).flashing("error" -> Messages("error.loading.organization"))
      }
  }

  def editPersonPhone(pid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>
      val result = Phone.getPersonPhone(Person.load(pid).toOption.get.get, id)
      if (result.isSuccess) {
        val countries = Country.getAll
        if (countries.isSuccess) {
          Ok(views.html.phoneForm(phoneForm.fill(result.toOption.get.get), countries.toOption.get, pid))
        } else {
          Ok(views.html.phoneForm(phoneForm.fill(result.toOption.get.get).withError("country", Messages("error.failedToLoadCountries")), List(), pid))
        }
      } else {
        Logger.error(result.toString, result.toEither.left.get)
        Redirect(routes.PhoneCtrl.showPersonPhone(pid)).flashing("error" -> Messages("error.failedToLoadAddress"))
      }
  }

  def showOrgPhone(oid: Long) = isAuthenticated { username =>
    implicit request =>
      val orgV = Organization.load(oid)
      orgV match {
        case Success(orgOp) => orgOp match {
          case Some(org) => val phonesV = Phone.getOrgPhones(org)
            phonesV match {
              case Success(phones) => Ok(views.html.phoneOrg(org, phones))
              case Failure(t) => Logger.error(phonesV.toString, t)
                Redirect(routes.OrganizationCtrl.show(oid)).flashing("error" -> Messages("error.loading.organization" +
                  ".phones"))
            }
          case None => Logger.error(s"Failed to load organization with ID '$oid'. Does not exist.")
            Redirect(routes.OrganizationCtrl.list()).flashing("error" -> Messages("error.loading.organization"))
        }
        case Failure(t) => Logger.error(orgV.toString, t)
          Redirect(routes.OrganizationCtrl.list()).flashing("error" -> Messages("error.loading.organization"))
      }
  }

  def showPersonPhone(pid: Long) = isAuthenticated { username =>
    implicit request =>
      val p = Person.load(pid).toOption.get.get
      val req = Ok(views.html.phone(p, Phone.getPersonPhones(p).toOption.get))
      if (request.flash.get("error").isDefined) {
        req.flashing("error" -> request.flash.get("error").get)
      } else if (request.flash.get("success").isDefined) {
        req.flashing("success" -> request.flash.get("success").get)
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
          val orgV = Organization.load(oid)
          orgV match {
            case Success(orgOp) => orgOp match {
              case Some(org) => val resultV = Phone.saveOrgPhone(org, ph)
                resultV match {
                  case Success(result) =>
                    Redirect(routes.PhoneCtrl.showOrgPhone(oid)).flashing("success" -> Messages("success.storing" +
                      ".organization.phone"))
                  case Failure(t) => Logger.error(resultV.toString, t)
                    Redirect(routes.OrganizationCtrl.show(oid)).flashing("error" -> Messages("error.storing" +
                      ".organization.phone"))
                }
              case None => Logger.error(s"Failed to load organization with ID '$oid'. Does not exist.")
                Redirect(routes.OrganizationCtrl.list()).flashing("error" -> Messages("error.loading" +
                  ".organization"))
            }
            case Failure(t) => Logger.error(orgV.toString, t)
              Redirect(routes.OrganizationCtrl.list()).flashing("error" -> Messages("error.loading.organization"))
          }
        }

      )
  }

  def submitPersonPhone(pid: Long) = isAuthenticated {
    username =>
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
            val p = Person.load(pid).toOption.get.get
            Logger.debug("Storing phone number " + ph._1.country.phone + " " + ph._1.areacode + " " + ph._1.extension + " for person " + p.lastname + ", " + p.firstname.getOrElse(""))
            val result = Phone.savePersonPhone(p, ph._1)
            if (result.isSuccess) {
              Redirect(routes.PhoneCtrl.showPersonPhone(pid)).flashing("success" -> Messages("success.succeededToStorePhone"))
            } else {
              val formWithError = phoneForm.withGlobalError(Messages("error.failedToStorePhone"))
              val countries = Country.getAll
              if (countries.isSuccess) {
                Logger.error(result.toString, result.toEither.left.get)
                BadRequest(views.html.phoneForm(formWithError, countries.toOption.get, pid))
              } else {
                BadRequest(views.html.phoneForm(formWithError.withError("country", Messages("error.failedToLoadCountries")), List(), pid))
              }
            }
          })
  }
}