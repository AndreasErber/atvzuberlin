/**
 *
 */
package controllers

import controllers.ext.ProvidesCtx
import controllers.ext.Security
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages
import play.api.Logger
import play.api.mvc._
import play.api.mvc.Controller
import play.api.Play.current
import util.CustomFormatters
import models.Email
import models.Charge
import util.Division
import models.PersonInCharges
import models.PersonInCharge
import models.PersonInCharge
import models.Person
import java.sql.Date

/**
 * @author andreas
 * @version 0.0.3, 2015-01-03
 */
object ChargeCtrl extends Controller with ProvidesCtx with Security {

  implicit val divisionFormatter = CustomFormatters.divisionFormatter
  val divisionMapping = of[Division.Division]

  implicit val personFormatter = CustomFormatters.personFormatter
  val personMapping = of[Person]

  implicit val chargeFormatter = CustomFormatters.chargeFormatter
  val chargeMapping = of[Charge]

  implicit val dateFormatter = CustomFormatters.sqlDateFormatter
  val dateMapping = of[Date]

  val chargeForm = Form[Charge](
    mapping(
      "id" -> optional(longNumber),
      "name" -> nonEmptyText,
      "abbr" -> optional(text),
      "division" -> divisionMapping,
      "shortDesc" -> optional(text),
      "longDesc" -> optional(text),
      "email" -> optional(email),
      "created" -> longNumber,
      "creator" -> text,
      "modified" -> optional(longNumber),
      "modifier" -> optional(text))(Charge.apply)(Charge.unapply))

  val piCForm = Form[PersonInCharge](
    mapping(
      "id" -> optional(longNumber),
      "person" -> personMapping,
      "charge" -> chargeMapping,
      "division" -> divisionMapping,
      "start" -> dateMapping,
      "end" -> dateMapping,
      "created" -> longNumber,
      "creator" -> text,
      "modified" -> optional(longNumber),
      "modifier" -> optional(text))(PersonInCharge.apply)(PersonInCharge.unapply))

  /**
   * Display an empty charge form to create a new {@link Charge}
   */
  def create = isAuthenticated { username =>
    implicit request =>
      Ok(views.html.chargeForm(chargeForm))
  }

  /**
   * Request the {@link Charge} identified by <em>id</em> to be deleted.
   *
   * @param id The identifier of the {@link Charge} to be deleted.
   */
  def delete(id: Long) = isAuthenticated { username =>
    implicit request =>
      val result = Charge.delete(id)
      if (result.isSuccess) {
        Logger.debug("Successfully deleted charge with ID " + id + ".")
        Redirect(routes.ChargeCtrl.list).flashing(("success" -> Messages("success.succeededToDeleteCharge")))
      } else {
        Logger.error(result.toString(), result.toEither.left.get)
        Redirect(routes.ChargeCtrl.list).flashing(("error" -> Messages("error.failedToDeleteCharge")))
      }
  }

  /**
   * Display a form pre-filled with the data of the {@link Charge} identified by <em>id</em>.
   *
   * @param id The identifier of the {@link Charge} to be edited.
   */
  def edit(id: Long) = isAuthenticated { username =>
    implicit request =>
      val c = Charge.load(id)
      c match {
        case None =>
          Logger.logger.debug("Cannot find charge with ID " + id + "."); NotFound
        case Some(charge) =>
          Logger.logger.debug("Preparing editing of charge with ID " + id + ".");
          Ok(views.html.chargeForm(chargeForm.fill(charge)))
        case _ => NotFound
      }
  }

  def list() = Action { implicit request =>

    val result = Charge.getAll()
    if (result.isSuccess) {
      val req = Ok(views.html.chargesList(result.toOption.get))
      if (request.flash.get("error").isDefined) {
        req.flashing(("error" -> request.flash.get("error").get))
      } else if (request.flash.get("success").isDefined) {
        req.flashing(("success" -> request.flash.get("success").get))
      } else {
        req
      }
    } else {
      Logger.error(result.toString(), result.toEither.left.get)
      Ok(views.html.chargesList(List())).flashing("error" -> Messages("error.failedToLoadChargesList"))
    }
  }

  def show(id: Long) = Action { implicit request =>
    val c = Charge.load(id)
    c match {
      case None =>
        Logger.logger.debug("No charge with ID " + id + " found."); NotFound
      case Some(ch) =>
        Logger.logger.debug("Found charge with ID " + id + ".")
        val req = Ok(views.html.charge(ch))
        if (request.flash.get("error").isDefined) {
          req.flashing(("error" -> request.flash.get("error").get))
        } else if (request.flash.get("success").isDefined) {
          req.flashing(("success" -> request.flash.get("success").get))
        } else {
          req
        }
      case _ => NotFound
    }
  }

  def submit = isAuthenticated { username =>
    implicit request =>
      chargeForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the charge form.")
          BadRequest(views.html.chargeForm(errors))
        },
        ch => {
          Logger.debug("Storing charge " + ch)
          val result = Charge.saveOrUpdate(ch)
          if (result.isSuccess) {
            Redirect(routes.ChargeCtrl.show(result.toOption.get.id.get)).flashing("success" -> Messages("success.succeededToStoreCharge"))
          } else {
            Logger.error(result.toString(), result.toEither.left.get)
            BadRequest(views.html.chargeForm(chargeForm)).flashing("error" -> Messages("error.failedToStoreCharge"))
          }
        })
  }

  /********************************************/

  /**
   * Display an empty charge form to create a new {@link PersonInCharge}
   */
  def createPiC(div: String) = isAuthenticated { username =>
    implicit request =>
      val persons = Person.getAll
      val charges = Charge.getAllForDivision(Division.withName(div))
      if (persons.isSuccess) {
        if (charges.isSuccess) {
          Ok(views.html.personInChargeForm(piCForm, persons.toOption.get, charges.toOption.get, div.toString()))
        } else {
          Ok(views.html.personInChargeForm(piCForm.withGlobalError(Messages("error.failedToLoadChargesList")), persons.toOption.get, List[Charge](), div.toString()))
        }
      } else {
        if (charges.isSuccess) {
          Ok(views.html.personInChargeForm(piCForm.withGlobalError(Messages("error.failedToLoadPersonList")), List[Person](), charges.toOption.get, div.toString()))
        } else {
          Ok(views.html.personInChargeForm(piCForm.withGlobalError(Messages("error.failedToLoadChargesList")), List[Person](), List[Charge](), div.toString()))
        }
      }
  }

  /**
   * Request the {@link PersonInCharge} identified by <em>id</em> to be deleted.
   *
   * @param id The identifier of the {@link PersonInCharge} to be deleted.
   */
  //  def deletePiC(id: Long) = isAuthenticated { username =>
  //    implicit request =>
  //      val result = Charge.delete(id)
  //      if (result.isSuccess) {
  //        Logger.debug("Successfully deleted charge with ID " + id + ".")
  //        Redirect(routes.ChargeCtrl.list).flashing(("success" -> Messages("success.succeededToDeleteCharge")))
  //      } else {
  //        Logger.error(result.toString(), result.toEither.left.get)
  //        Redirect(routes.ChargeCtrl.list).flashing(("error" -> Messages("error.failedToDeleteCharge")))
  //      }
  //  }

  /**
   * Display a form pre-filled with the data of the {@link PersonInCharge} identified by <em>id</em>.
   *
   * @param id The identifier of the {@link PersonInCharge} to be edited.
   */
  def editPiC(id: Long) = isAuthenticated { username =>
    implicit request =>
      val c = PersonInCharges.get(id)
      c match {
        case None =>
          Logger.logger.debug("Cannot find person in charge with ID " + id + "."); NotFound
        case Some(pic) =>
          Logger.logger.debug("Preparing editing of person in charge with ID " + id + ".");
          val persons = Person.getAll
          val charges = Charge.getAllForDivision(c.get.division)
          // TODO: error handling
          Ok(views.html.personInChargeForm(piCForm.fill(pic), persons.toOption.get, charges.toOption.get, c.get.division.toString()))
        case _ => NotFound
      }
  }

  def listPiC(div: String) = Action { implicit request =>

    val result = PersonInCharges.getAll
    if (result.isSuccess) {
      val req = Ok(views.html.personInChargeList(result.toOption.get))
      if (request.flash.get("error").isDefined) {
        req.flashing(("error" -> request.flash.get("error").get))
      } else if (request.flash.get("success").isDefined) {
        req.flashing(("success" -> request.flash.get("success").get))
      } else {
        req
      }
    } else {
      Logger.error(result.toString(), result.toEither.left.get)
      Ok(views.html.personInChargeList(List())).flashing("error" -> Messages("error.failedToLoadPersonInChargeList"))
    }
  }

  //  def showPiC(id: Long) = Action { implicit request =>
  //    val c = PersonInCharges.get(id)
  //    c match {
  //      case None =>
  //        Logger.logger.debug("No charge with ID " + id + " found."); NotFound
  //      case Some(ch) =>
  //        Logger.logger.debug("Found charge with ID " + id + ".")
  //        val req = Ok(views.html.charge(ch))
  //        if (request.flash.get("error").isDefined) {
  //          req.flashing(("error" -> request.flash.get("error").get))
  //        } else if (request.flash.get("success").isDefined) {
  //          req.flashing(("success" -> request.flash.get("success").get))
  //        } else {
  //          req
  //        }
  //      case _ => NotFound
  //    }
  //  }

  def submitPiC = isAuthenticated { username =>
    implicit request =>
      val persons = Person.getAll
      // TODO: error handling
      piCForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the PersonInCharge form.")
          val div = Division.withName(errors.data.get("division").get)
          val charges = Charge.getAllForDivision(div)
          // TODO: error handling
          BadRequest(views.html.personInChargeForm(errors, persons.toOption.get, charges.toOption.get, div.toString()))
        },
        pic => {
          Logger.debug("Storing person in charge " + pic)
          val result = PersonInCharges.saveOrUpdate(pic)
          if (result.isSuccess) {
            Redirect(routes.ChargeCtrl.listPiC(pic.division.toString())).flashing("success" -> Messages("success.succeededToStorePersonInCharge"))
          } else {
            Logger.error(result.toString(), result.toEither.left.get)
            val charges = Charge.getAllForDivision(pic.division)
            // TODO: error handling
            BadRequest(views.html.personInChargeForm(piCForm, persons.toOption.get, charges.toOption.get, pic.division.toString())).flashing("error" -> Messages("error.failedToStorePersonInCharge"))
          }
        })
  }

}