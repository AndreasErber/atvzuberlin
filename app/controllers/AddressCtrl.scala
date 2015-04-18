/**
 *
 */
package controllers

import controllers.ext.{ProvidesCtx, Security}
import models.{Address, Country, Organization, Person, PersonHasAddresses}
import play.api.data.Form
import play.api.data.Forms._
import play.api.Logger
import play.api.mvc._
import play.api.i18n.Messages
import util.{CustomFormatters, Privacy, UsageType}

/**
 * Controller for all actions related to [[Address]]es.
 *
 * @author andreas
 * @version 0.0.9, 2015-04-18
 */
object AddressCtrl extends Controller with ProvidesCtx with Security {

  implicit val countryFormatter = CustomFormatters.countryFormatter
  val countryMapping = of[Country]

  implicit val usageFormatter = CustomFormatters.usageTypeFormatter
  val usageMapping = of[UsageType]

  implicit val privacyFormatter = CustomFormatters.privacyFormatter
  val privacyMapping = of[Privacy]

  /**
   * Mapping from an address form to an [[Address]].
   */
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

  /**
   * An address form for a [[Person]].
   */
  val adrPersForm = Form[(Address, UsageType, Privacy)](
    tuple(
      "address" -> adrMapping,
      "usage" -> usageMapping,
      "privacy" -> privacyMapping))

  /**
   * An address form for an [[Organization]].
   */
  val adrOrgForm = Form[Address](adrMapping)

  /**
   * Create a new [[Address]] for a [[Person]] identified by the given <em>pid</em>.
   *
   * @param pid Identifier of the [[Person]] the [[Address]] relates to.
   * @return A response with HTTP status code 200 and an empty person address form. If the list of
   *         countries cannot be loaded a redirect to display the details of the [[Person]]
   *         identified by <em>pid</em> is returned.
   */
  def createPersonAdr(pid: Long) = isAuthorized("create.person.address") { username =>
    implicit request =>
      val countries = Country.getAll()
      if (countries.isSuccess) {
        Ok(views.html.adrForm(adrPersForm.bind(Map("usage" -> "1", "privacy" -> "2")).discardingErrors, countries.toOption.get, pid))
      } else {
        Logger.error("Failed to load the list of countries.")
        Redirect(routes.PersonCtrl.show(pid)).flashing("error" -> Messages("error.loading.countries"))
      }
  }

  /**
   * Create a new [[Address]] for an [[Organization]] identified by the given <em>oid</em>.
   *
   * @param oid Identifier of the [[Organization]] the [[Address]] relates to.
   * @return A response with HTTP status code 200 and an empty person address form. If the list of
   *         countries cannot be loaded a redirect to display the details of the [[Organization]]
   *         identified by <em>oid</em> is returned.
   */
  def createOrgAdr(oid: Long) = isAuthorized("create.organization.address") { username =>
    implicit request =>
      val countries = Country.getAll()
      if (countries.isSuccess) {
        Ok(views.html.adrOrgForm(adrOrgForm.bind(Map("usage" -> "1", "privacy" -> "2")).discardingErrors, countries.toOption.get, oid))
      } else {
        Logger.error("Failed to load the list of countries.")
        Redirect(routes.OrganizationCtrl.show(oid)).flashing("error" -> Messages("error.loading.countries"))
      }
  }

  /**
   * Delete an [[Address]] identified by <em>id</em> of an [[Organization]] identified by <em>oid</em>.
   *
   * @param oid Identifier of the [[Organization]] the [[Address]] relates to.
   * @param id Identifier of the [[Address]] to delete.
   * @return A redirect to the details of an [[Organization]] identified <em>oid</em> flashing either success or error.
   */
  def deleteOrgAdr(oid: Long, id: Long) = isAuthorized("delete.organization.address") { username =>
    implicit request =>
      val result = Address.deleteOrgAddress(id, id)
      if (result.isSuccess) {
        Logger.info(s"Successfully deleted the relation between address '$id' and person '$oid'.")
        Redirect(routes.OrganizationCtrl.show(oid)).flashing("success" -> Messages("success.deleting.address"))
      } else {
        Logger.error(result.toString, result.toEither.left.get)
        Redirect(routes.OrganizationCtrl.show(oid)).flashing("error" -> Messages("error.deleting.address"))
      }
  }

  /**
   * Delete an [[Address]] identified by <em>id</em> of an [[Person]] identified by <em>pid</em>.
   *
   * @param pid Identifier of the [[Person]] the [[Address]] relates to.
   * @param id Identifier of the [[Address]] to delete.
   * @return A redirect to the details of an [[Person]] identified <em>pid</em> flashing either success or error.
   */
  def deletePersonAdr(pid: Long, id: Long) = isAuthorized("delete.person.address") { username =>
    implicit request =>
      val result = PersonHasAddresses.delete(pid, id)
      if (result.isSuccess) {
        Logger.info(s"Successfully deleted the relation between address '$id' and person '$pid'.")
        Redirect(routes.PersonCtrl.show(pid)).flashing("success" -> Messages("success.deleting.address"))
      } else {
        Logger.error(result.toString, result.toEither.left.get)
        Redirect(routes.PersonCtrl.show(pid)).flashing("error" -> Messages("error.deleting.address"))
      }
  }

  /**
   * Edit an [[Address]] identified by <em>id</em> of an [[Organization]] identified by <em>oid</em>
   *
   * @param oid Identifier of the [[Organization]] the [[Address]] relates to.
   * @param id Identifier of the [[Address]] to edit.
   * @return A response with an HTTP status code of 200 and the requested [[Address]] details in a
   *         form. If either the [[Address]] itself or the list of [[Country]]s cannot be loaded a
   *         redirect to display the details of the [[Organization]] is returned.
   */
  def editOrgAdr(oid: Long, id: Long) = isAuthorized("edit.organization.address") { username =>
    implicit request =>

      val result = Address.getOrgAddress(Organization.load(oid).get, id)
      if (result.isSuccess) {
        val countries = Country.getAll()
        if (countries.isSuccess) {
          Ok(views.html.adrOrgForm(adrOrgForm.fill(result.toOption.get.get), countries.toOption.get, oid))
        } else {
          Logger.error("Failed to load the list of countries.")
          Redirect(routes.OrganizationCtrl.show(oid)).flashing("error" -> Messages("error.loading.countries"))
        }
      } else {
        Logger.error(result.toString, result.toEither.left.get)
        Redirect(routes.OrganizationCtrl.show(oid)).flashing("error" -> Messages("error.loading.organization.address"))
      }
  }

  /**
   * Edit an [[Address]] identified by <em>id</em> of a [[Person]] identified by <em>pid</em>
   *
   * @param pid Identifier of the [[Person]] the [[Address]] relates to.
   * @param id Identifier of the [[Address]] to edit.
   * @return A response with an HTTP status code of 200 and the requested [[Address]] details in a
   *         form. If either the [[Address]] itself or the list of [[Country]]s cannot be loaded a
   *         redirect to display the details of the [[Person]] is returned.
   */
  def editPersonAdr(pid: Long, id: Long) = isAuthorized("edit.person.address") { username =>
    implicit request =>

      val result = Address.getPersonAddress(Person.load(pid).get, id)
      if (result.isSuccess) {
        val countries = Country.getAll()
        if (countries.isSuccess) {
          Ok(views.html.adrForm(adrPersForm.fill(result.toOption.get.get), countries.toOption.get, pid))
        } else {
          Logger.error("Failed to load the list of countries.")
          Redirect(routes.AddressCtrl.showPersonAdr(pid)).flashing("error" -> Messages("error.loading.countries"))
        }
      } else {
        Logger.error(result.toString, result.toEither.left.get)
        Redirect(routes.AddressCtrl.showPersonAdr(pid)).flashing("error" -> Messages("error.loading.person.address"))
      }
  }

  /**
   * Display the [[Address]]es of an [[Organization]].
   *
   * @param oid Identifier of the [[Organization]] to display the [[Address]]es for.
   * @return A response with HTTP status code 200 having the [[Address]]es for the specific
   *         [[Organization]] for a payload. If the [[Address]]es cannot be loaded a redirect to
   *         the details of the [[Organization]] is returned. If the [[Organization]] cannot be
   *         found a redirect to the list of [[Organization]]s is returned.
   */
  def showOrgAdr(oid: Long) = isAuthenticated { username =>
    implicit request =>
      val o = Organization.load(oid)
      if (o.isDefined) {
        val orgAddresses = Address.getOrgAddresses(o.get)
        if (orgAddresses.isSuccess) {
          Ok(views.html.adrOrg(o.get, orgAddresses.toOption.get))
        } else {
          Logger.error(s"Failed to load addresses for organization '$oid")
          Redirect(routes.OrganizationCtrl.show(oid)).flashing("error" -> Messages("error.loading.organization.addresses"))
        }
      } else {
        Logger.error(s"Failed to load organization '$oid'.")
        Redirect(routes.OrganizationCtrl.list()).flashing("error" -> Messages("error.loading.organization"))
      }
  }

  /**
   * Display the [[Address]]es of a [[Person]].
   *
   * @param pid Identifier of the [[Person]] to display the [[Address]]es for.
   * @return A response with HTTP status code 200 having the [[Address]]es for the specific
   *         [[Person]] for a payload. If the [[Address]]es cannot be loaded a redirect to
   *         the details of the [[Person]] is returned. If the [[Person]] cannot be
   *         found a redirect to the list of [[Person]]s is returned.
   */
  def showPersonAdr(pid: Long) = isAuthenticated { username =>
    implicit request =>
      val p = Person.load(pid)
      if (p.isDefined) {
        val persAddresses = Address.getPersonAddresses(p.get)
        if (persAddresses.isSuccess) {
          Ok(views.html.adr(p.get, persAddresses.toOption.get))
        } else {
          Logger.error(s"Failed to load addresses for person '$pid'.")
          Redirect(routes.PersonCtrl.show(pid)).flashing("error" -> Messages("error.loading.person.addresses"))
        }
      } else {
        Logger.error(s"Failed to load person '$pid'.")
        Redirect(routes.PersonCtrl.list()).flashing("error" -> Messages("error.loading.person", pid))
      }
  }

  /**
   * Store an [[Address]] of an [[Organization]].
   *
   * @param oid Identifier of the [[Organization]] to relate the [[Address]] to.
   * @return A redirect to display the [[Address]]es for the specific [[Organization]]. If storing
   *         the address fails the details of the [[Organization]] are displayed. If form
   *         validation fails a response with HTTP status code 400 is generated holding the form
   *         with error information. If the [[Organization]] cannot be loaded a redirect to the list
   *         of [[Organization]]s is returned.
   */
  def submitOrgAdr(oid: Long) = isAuthorized("save.organization.address") { username =>
    implicit request =>
      adrOrgForm.bindFromRequest.fold(
        errors => {
          Logger.error(s"An error occurred when trying to process organization address form for organization '$oid'.")
          val countries = Country.getAll()
          if (countries.isSuccess) {
            BadRequest(views.html.adrOrgForm(errors, countries.toOption.get, oid))
          } else {
            BadRequest(views.html.adrOrgForm(errors.withError("country", Messages("error.loading.countries")), List(), oid))
          }
        },
        adr => {
          val o = Organization.load(oid)
          if (o.isDefined) {
            Logger.debug(s"Storing address $adr for organization '${o.get.name}.")
            val result = Address.saveOrgAddress(o.get, adr)
            if (result.isSuccess) {
              Redirect(routes.AddressCtrl.showOrgAdr(oid)).flashing("success" -> Messages("success.storing.organization.address"))
            } else {
              Logger.error(result.toString, result.toEither.left.get)
              Redirect(routes.OrganizationCtrl.show(oid)).flashing("error" -> Messages("error.storing.organization.address"))
            }
          } else {
            Logger.error(s"Failed to load organization '$oid'. Cannot store address for it.")
            Redirect(routes.OrganizationCtrl.list()).flashing("error" -> Messages("error.loading.organization"))
          }
        })
  }

  /**
   * Store an [[Address]] of an [[Person]].
   *
   * @param pid Identifier of the [[Person]] to relate the [[Address]] to.
   * @return A redirect to display the [[Address]]es for the specific [[Person]]. If storing
   *         the address fails the details of the [[Person]] are displayed. If form
   *         validation fails a response with HTTP status code 400 is generated holding the form
   *         with error information. If the [[Person]] cannot be loaded a redirect to the list
   *         of [[Person]]s is returned.
   */
  def submitPersonAdr(pid: Long) = isAuthorized("save.person.address") { username =>
    implicit request =>
      adrPersForm.bindFromRequest.fold(
        errors => {
          Logger.error(s"An error occurred when trying to process person address form for person '$pid'.")
          val countries = Country.getAll()
          if (countries.isSuccess) {
            BadRequest(views.html.adrForm(errors, countries.toOption.get, pid))
          } else {
            BadRequest(views.html.adrForm(errors.withError("country", Messages("error.loading.countries")), List(), pid))
          }
        },
        adr => {
          val p = Person.load(pid)
          if (p.isDefined) {
            Logger.debug(s"Storing address $adr for person '${p.get.fullname}'.")
            val result = Address.savePersonAddress(p.get, adr)
            if (result.isSuccess) {
              Redirect(routes.AddressCtrl.showPersonAdr(pid)).flashing("success" -> Messages("success.storing.person.address"))
            } else {
              Logger.error(result.toString, result.toEither.left.get)
              Redirect(routes.PersonCtrl.show(pid)).flashing("error" -> Messages("error.storing.person.address"))
            }
          } else {
            Logger.error(s"Failed to load person '$pid'. Cannot store address for it.")
            Redirect(routes.PersonCtrl.list()).flashing("error" -> Messages("error.loading.person"))
          }
        })
  }
}