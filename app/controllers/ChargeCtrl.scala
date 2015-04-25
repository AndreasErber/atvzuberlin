/**
 *
 */
package controllers

import controllers.ext.{ProvidesCtx, Security}
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages
import play.api.Logger
import play.api.mvc._
import play.api.mvc.Controller
import util.{CustomFormatters, Division}
import models.{Charge, Person, PersonInCharge, PersonInCharges}
import java.sql.Date

import scalaz.{Success, Failure}

/**
 * Controller to handle requests on [[Charge]]s.
 *
 * @author andreas
 * @version 0.0.6, 2015-04-25
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

  /**
   * Form to handle [[Charge]] data.
   */
  val chargeForm = Form[Charge](
    mapping(
      "id" -> optional(longNumber),
      "nameMale" -> nonEmptyText,
      "nameFemale" -> nonEmptyText,
      "abbr" -> optional(text),
      "division" -> divisionMapping,
      "position" -> number,
      "shortDesc" -> optional(text),
      "longDesc" -> optional(text),
      "emailMale" -> optional(email),
      "emailFemale" -> optional(email),
      "created" -> longNumber,
      "creator" -> text,
      "modified" -> optional(longNumber),
      "modifier" -> optional(text))(Charge.apply)(Charge.unapply))

  /**
   * Form to handle [[PersonInCharge]] data.
   */
  val piCForm = Form[PersonInCharge](
    mapping(
      "id" -> optional(longNumber),
      "person" -> personMapping,
      "charge" -> chargeMapping,
      "start" -> dateMapping,
      "end" -> optional(dateMapping),
      "created" -> longNumber,
      "creator" -> text,
      "modified" -> optional(longNumber),
      "modifier" -> optional(text))(PersonInCharge.apply)(PersonInCharge.unapply))

  /**
   * Display an empty form to create a new [[Charge]].
   *
   * @return A response with HTTP status code 200 and an empty [[Charge]] form.
   */
  def create = isAuthorized("create.charge") { username =>
    implicit request =>
      Ok(views.html.chargeForm(chargeForm))
  }

  /**
   * Request the [[Charge]] identified by <em>id</em> to be deleted.
   *
   * @param id The identifier of the [[Charge]] to be deleted.
   * @return A redirect to the list of [[Charge]]s flashing either success or failure.
   */
  def delete(id: Long) = isAuthorized("delete.charge") { username =>
    implicit request =>
      val result = Charge.delete(id)
      if (result.isSuccess) {
        Logger.debug(s"Successfully deleted charge with ID $id.")
        Redirect(routes.ChargeCtrl.list()).flashing("success" -> Messages("success.deleting.charge"))
      } else {
        Logger.error(result.toString, result.toEither.left.get)
        Redirect(routes.ChargeCtrl.list()).flashing("error" -> Messages("error.deleting.charge"))
      }
  }

  /**
   * Display a form pre-filled with the data of the [[Charge]] identified by <em>id</em>.
   *
   * @param id The identifier of the [[Charge]] to be edited.
   * @return A response with HTTP status code 200 and the [[Charge]] form preloaded with the
   *         specific data for a payload. In case the [[Charge]] cannot be loaded a redirect
   *         to the list of [[Charge]]s is returned.
   */
  def edit(id: Long) = isAuthorized("edit.charge") { username =>
    implicit request =>
      val chargeV = Charge.load(id)
      if (chargeV.isSuccess) {
        val chargeOp = chargeV.toOption.get
        chargeOp match {
          case None =>
            Logger.error(s"Cannot find charge with ID '$id'. Does not exist.")
            Redirect(routes.ChargeCtrl.list()).flashing("error" -> Messages("error.loading.charge"))
          case Some(charge) =>
            Logger.debug(s"Preparing editing of charge with ID '$id'.")
            Ok(views.html.chargeForm(chargeForm.fill(charge)))
          case _ => NotFound
        }
      } else {
        Logger.error(chargeV.toString, chargeV.toEither.left.get)
        Redirect(routes.ChargeCtrl.list()).flashing("error" -> Messages("error.loading.charge"))
      }
  }

  /**
   * Display the list of [[Charge]]s.
   *
   * @return A response with HTTP status code 200 with the list of all [[Charge]]s for a payload.
   *         If the list of [[Charge]]s cannot be loaded a response with an HTTP status code 400
   *         and an error message is returned.
   */
  def list() = isAuthorized("view.charge") { username =>
    implicit request =>
      val resultV = Charge.getAll
      resultV match {
        case Success(result) =>
          Ok(views.html.chargesList(result))
        case Failure(t) => Logger.error(resultV.toString, t)
          Redirect(routes.Application.administration()).flashing("error" -> Messages("error.loading.charges"))
      }
  }

  /**
   * Show the [[Charge]] identified by the given <em>id</em>.
   *
   * @param id The identifier of the [[Charge]] to display.
   * @return A response with HTTP status code 200 holding the details of the specific [[Charge]].
   *         If the [[Charge]] cannot be loaded a redirect to the list of [[Charge]]s is returned.
   */
  def show(id: Long) = isAuthorized("view.charge") { username =>
    implicit request =>
      val chargeV = Charge.load(id)
      chargeV match {
        case Success(chargeOp) => chargeOp match {
          case Some(charge) =>
            Logger.debug(s"Found charge with ID '$id'.")
            Ok(views.html.charge(charge))
          case None =>
            Logger.debug(s"No charge with ID '$id' found. Does not exist")
            Redirect(routes.ChargeCtrl.list()).flashing("error" -> Messages("error.loading.charge"))
        }
        case Failure(t) => Logger.error(chargeV.toString, t)
          Redirect(routes.ChargeCtrl.list()).flashing("error" -> Messages("error.loading.charge"))
      }
  }

  /**
   * Submit the form data.
   *
   * @return A redirect to display the details of the [[Charge]]. If storing the data or form
   *         validation fails the form is redisplayed with error information.
   */
  def submit = isAuthorized("save.charge") { username =>
    implicit request =>
      chargeForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the charge form.")
          BadRequest(views.html.chargeForm(errors))
        },
        charge => {
          Logger.debug(s"Storing charge $charge.")
          val result = Charge.saveOrUpdate(charge)
          if (result.isSuccess) {
            Redirect(routes.ChargeCtrl.show(result.toOption.get.id.get)).flashing("success" -> Messages("success.storing.charge"))
          } else {
            Logger.error(result.toString, result.toEither.left.get)
            BadRequest(views.html.chargeForm(chargeForm)).flashing("error" -> Messages("error.storing.charge"))
          }
        })
  }

  /**
   * Display an empty form to create a new [[PersonInCharge]].
   *
   * @return A response with HTTP status code 200 carrying a [[PersonInCharge]] form to create a
   *         new relation between a [[Person]] and a [[Charge]]. If one or both of the [[Person]]
   *         or the [[Charge]] list cannot be loaded a redirect to the administration overview
   *         page is returend.
   */
  def createPiC(div: String) = isAuthorized("create.person.in.charge") { username =>
    implicit request =>
      val persons = Person.getAll
      if (persons.isSuccess) {
        val charges = Charge.getAllForDivision(Division.withName(div))
        if (charges.isSuccess) {
          Ok(views.html.personInChargeForm(piCForm, persons.toOption.get, charges.toOption.get))
        } else {
          Logger.error("Failed to load list of charges.")
          Redirect(routes.Application.administration()).flashing("error" -> Messages("error.loading.charges"))
        }
      } else {
        Logger.error("Failed to load list of persons.")
        Redirect(routes.Application.administration()).flashing("error" -> Messages("error.loading.persons"))
      }
  }

  /**
   * Request the [[PersonInCharge]] identified by <em>id</em> to be deleted.
   *
   * @param id The identifier of the [[PersonInCharge]] to be deleted.
   * @return A redirect to the [[Charge]] list flashing either success or error.
   */
  def deletePiC(id: Long) = isAuthorized("delete.person.in.charge") { username =>
    implicit request =>
      val result = PersonInCharges.delete(id)
      if (result > 0) {
        Logger.debug(s"Successfully deleted person in charge relation with ID '$id'.")
        Redirect(routes.ChargeCtrl.list()).flashing("success" -> Messages("success.deleting.person.in.charge"))
      } else {
        Logger.error(s"Failed to delete person in charge relation with ID '$id'")
        Redirect(routes.ChargeCtrl.list()).flashing("error" -> Messages("error.deleting.person.in.charge"))
      }
  }

  /**
   * Display a form pre-filled with the data of the [[PersonInCharge]] identified by <em>id</em>.
   *
   * @param id The identifier of the [[PersonInCharge]] to be edited.
   * @return A response with HTTP status code 200 carrying a form pre-filled with the requested data. If the
   *         [[PersonInCharge]] cannot be found, the list of [[Person]]s or [[Charge]]s cannot be loaded then a
   *         redirect to the administration overview page is returned.
   */
  def editPiC(id: Long) = isAuthorized("edit.person.in.charge") { username =>
    implicit request =>
      val picV = PersonInCharges.get(id)
      picV match {
        case Success(picOp) => picOp match {
          case Some(pic) => Logger.debug(s"Preparing editing of person in charge with ID '$id'.")
            val personsV = Person.getAll
            personsV match {
              case Success(persons) => val chargesV = Charge.getAll
                chargesV match {
                  case Success(charges) =>
                    Ok(views.html.personInChargeForm(piCForm.fill(pic), personsV.toOption.get, chargesV.toOption.get))
                  case Failure(t) => Logger.error(chargesV.toString, t)
                    Redirect(routes.Application.administration()).flashing("error" -> Messages("error.loading.charges"))
                }
              case Failure(t) => Logger.error(personsV.toString, t)
                Redirect(routes.Application.administration()).flashing("error" -> Messages("error.loading.persons"))
            }
          case None =>
            Logger.debug(s"Cannot find person in charge with ID '$id'.")
            Redirect(routes.Application.administration()).flashing("error" -> Messages("error.loading.person.in.charge"))
        }
        case Failure(t) => Logger.error(picV.toString, t)
          Redirect(routes.Application.administration()).flashing("error" -> Messages("error.loading.person.in.charge"))
      }
  }

  /**
   * Display the [[PersonInCharge]] list of the given organization division.
   *
   * @param div The name of the division to display the list for.
   * @return A response with HTTP status code 200 holding all the [[PersonInCharge]] entries for the given division.
   *         In case of error a redirect to the administration overview page is returned.
   */
  def listPiC(div: String) = isAuthorized("view.person.in.charge") { username =>
  implicit request =>
    val picV = PersonInCharges.getAllCurrentByDivision(div)
    picV match {
      case Success(pics) => Ok(views.html.personInChargeList(pics.sortBy(_.charge.position), div))
      case Failure(t) => Logger.error(picV.toString, t)
        Redirect(routes.Application.administration()).flashing("error" -> Messages("error.loading.person.in.charges"))
    }
  }

  /**
   * Submit the form data.
   *
   * @return
   */
  def submitPiC = isAuthenticated { username =>
    implicit request =>
      piCForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the PersonInCharge form.")
          val persons = Person.getAll
          if (persons.isSuccess) {
            //            val div = Division.withName(errors.data.get("division").get)
            val charges = Charge.getAll
            if (charges.isSuccess) {
              BadRequest(views.html.personInChargeForm(errors, persons.toOption.get, charges.toOption.get))
            } else {
              Logger.error("Failed to load the list of charges.")
              Redirect(routes.Application.administration()).flashing("error" -> Messages("error.loading.charges"))
            }
          } else {
            Logger.error("Failed to load the list of persons.")
            Redirect(routes.Application.administration()).flashing("error" -> Messages("error.loading.persons"))
          }
        },
        pic => {
          Logger.debug(s"Storing person in charge $pic.")
          val result = PersonInCharges.saveOrUpdate(pic)
          if (result.isSuccess) {
            Redirect(routes.ChargeCtrl.listPiC(pic.charge.division.toString)).flashing("success" -> Messages("success" +
              ".storing.person.in.charge"))
          } else {
            Logger.error(result.toString, result.toEither.left.get)
            val persons = Person.getAll
            if (persons.isSuccess) {
              val charges = Charge.getAllForDivision(pic.charge.division)
              if (charges.isSuccess) {
                BadRequest(views.html.personInChargeForm(piCForm, persons.toOption.get, charges.toOption.get)).
                  flashing("error" -> Messages("error.storing.person.in.charge"))
              } else {
                Logger.error("Failed to load the list of charges.")
                Redirect(routes.Application.administration()).flashing("error" -> Messages("error.loading.charges"))
              }
            } else {
              Logger.error("Failed to load the list of persons.")
              Redirect(routes.Application.administration()).flashing("error" -> Messages("error.loading.persons"))
            }
          }
        })
  }
}