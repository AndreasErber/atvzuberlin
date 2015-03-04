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
import models.{ Address, Addresses, Country, Organization, Person, PersonHasEmail }
import play.api.Play.current
import controllers.ext.ProvidesCtx
import controllers.ext.Security
import play.api.i18n.Messages
import util.Privacy
import models.PersonHasAddresses

/**
 * Controller for all actions related to addresses.
 * 
 * @author andreas
 * @version 0.0.8, 2015-01-03
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

  /**
   * Create a new {@link Address} for a {@link Person} identified by the given <em>pid</em>.
   * 
   * @param pid Identifier of the {@link Person} the {@link Address} relates to.
   */
  def createPersonAdr(pid: Long) = isAuthorized("create.person.address") { username =>
    implicit request =>
      val countries = Country.getAll
      if (countries.isSuccess) {
        Ok(views.html.adrForm(adrPersForm.bind(Map("usage" -> "1", "privacy" -> "2")).discardingErrors, countries.toOption.get, pid))
      } else {
        Ok(views.html.adrForm(adrPersForm.withGlobalError(Messages("error.failedToLoadCountries")), List(), pid))
      }
  }

  /**
   * Create a new {@link Address} for an {@link Organization} identified by the given <em>oid</em>.
   * 
   * @param oid Identifier of the {@link Organization} the {@link Address} relates to.
   */
  def createOrgAdr(oid: Long) = isAuthorized("create.organization.address") { username =>
    implicit request =>
      val countries = Country.getAll
      if (countries.isSuccess) {
        Ok(views.html.adrOrgForm(adrOrgForm.bind(Map("usage" -> "1", "privacy" -> "2")).discardingErrors, countries.toOption.get, oid))
      } else {
        Ok(views.html.adrOrgForm(adrOrgForm.withGlobalError(Messages("error.failedToLoadCountries")), List(), oid))
      }
  }

  /**
   * Delete an address identified by <em>id</em> of an {@link Organization} identified by <em>oid</em>.
   * 
   * @param oid Identifier of the {@link Organization} the address relates to.
   * @param id Identifier of the {@link Address} to delete.
   */
  def deleteOrgAdr(oid: Long, id: Long) = isAuthorized("delete.organization.address") { username =>
    implicit request =>
      val result = Address.deleteOrgAddress(id, id)
      if (result.isSuccess) {
        Redirect(routes.AddressCtrl.showOrgAdr(oid)).flashing(("success" -> Messages("success.succeededToDeleteAddress")))
      } else {
        Logger.error(result.toString(), result.toEither.left.get)
        Redirect(routes.AddressCtrl.showOrgAdr(oid)).flashing(("error" -> Messages("error.failedToDeleteAddress")))
      }
  }

  /**
   * Delete an {@link Address} identified by <em>id</em> of an {@link Person} identified by <em>pid</em>.
   * 
   * @param pid Identifier of the {@link Person} the {@link Address} relates to.
   * @param id Identifier of the {@link Address} to delete.
   */
  def deletePersonAdr(pid: Long, id: Long) = isAuthorized("delete.person.address") { username =>
    implicit request =>
      val result = PersonHasAddresses.delete(pid, id)
      if (result.isSuccess) {
        Redirect(routes.AddressCtrl.showPersonAdr(pid)).flashing(("success" -> Messages("success.succeededToDeleteAddress")))
      } else {
        Logger.error(result.toString(), result.toEither.left.get)
        Redirect(routes.AddressCtrl.showPersonAdr(pid)).flashing(("error" -> Messages("error.failedToDeleteAddress")))
      }
  }

  /**
   * Edit an {@link Address} identified by <em>id</em> of an {@link Organization} identified by <em>oid</em>
   * 
   *  @param oid Identifier of the {@link Organization} the {@link Address} relates to.
   *  @param id Identifier of the {@link Address} to edit.
   */
  def editOrgAdr(oid: Long, id: Long) = isAuthorized("edit.organization.address") { username =>
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
        Logger.error(result.toString(), result.toEither.left.get)
        Redirect(routes.AddressCtrl.showOrgAdr(oid)).flashing(("error" -> Messages("error.failedToLoadAddress")))
      }
  }

  /**
   * Edit an {@link Address} identified by <em>id</em> of a {@link Person} identified by <em>pid</em>
   * 
   *  @param pid Identifier of the {@link Person} the {@link Address} relates to.
   *  @param id Identifier of the {@link Address} to edit.
   */
  def editPersonAdr(pid: Long, id: Long) = isAuthorized("edit.person.address") { username =>
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
        Logger.error(result.toString(), result.toEither.left.get)
        Redirect(routes.AddressCtrl.showPersonAdr(pid)).flashing(("error" -> Messages("error.failedToLoadAddress")))
      }
  }

  /**
   * Display the {@link Address}es of an {@link Organization}.
   * 
   * @param oid Identifier of the {@link Organization} to display the {@link Address}es for.
   */
  def showOrgAdr(oid: Long) = isAuthenticated { username =>
    implicit request =>
      val o = Organization.load(oid).get
      val req = Ok(views.html.adrOrg(o, Address.getOrgAddresses(o).toOption.get))
      if (request.flash.get("error").isDefined) {
        req.flashing(("error" -> request.flash.get("error").get))
      } else if (request.flash.get("success").isDefined) {
        req.flashing(("success" -> request.flash.get("success").get))
      } else {
        req
      }
  }

  /**
   * Display the {@link Address}es of a {@link Person}.
   * 
   * @param pid Identifier of the {@link Person} to display the {@link Address}es for.
   */
  def showPersonAdr(pid: Long) = isAuthenticated { username =>
    implicit request =>
      val p = Person.load(pid).get
      val req = Ok(views.html.adr(p, Address.getPersonAddresses(p).toOption.get))
      if (request.flash.get("error").isDefined) {
        req.flashing(("error" -> request.flash.get("error").get))
      } else if (request.flash.get("success").isDefined) {
        req.flashing(("success" -> request.flash.get("success").get))
      } else {
        req
      }
  }

  /**
   * Store an {@link Address} of an {@link Organization}.
   * 
   * @param oid Identifier of the {@link Organization} to relate the {@link Address} to.
   */
  def submitOrgAdr(oid: Long) = isAuthorized("save.organization.address") { username =>
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
            Logger.error(result.toString(), result.toEither.left.get)
            val countries = Country.getAll
            if (countries.isSuccess) {
              BadRequest(views.html.adrOrgForm(formWithError, countries.toOption.get, oid))
            } else {
              BadRequest(views.html.adrOrgForm(formWithError.withError("country", Messages("error.failedToLoadCountries")), List(), oid))
            }
          }
        })
  }

  /**
   * Store an {@link Address} of an {@link Person}.
   * 
   * @param pid Identifier of the {@link Person} to relate the {@link Address} to.
   */
  def submitPersonAdr(pid: Long) = isAuthorized("save.person.address") { username =>
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
            Logger.error(result.toString(), result.toEither.left.get)
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