/**
 *
 */
package controllers

import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages
import play.api.Logger
import play.api.mvc._
import play.api.Play.current
import controllers.ext.ProvidesCtx
import controllers.ext.Security
import util.CustomFormatters
import models.{ Enrollment, Event, Person }
import scalaz.Success

/**
 * @author andreas
 * @version 0.0.1, 2013-04-28
 */
object EnrollmentCtrl extends Controller with ProvidesCtx with Security {

  val enrollmentForm = Form[Enrollment] {
    mapping(
      "id" -> optional(longNumber),
      "event" -> longNumber,
      "person" -> longNumber,
      "numberOfAdults" -> number,
      "numberOfKids" -> number,
      "confirmed" -> boolean,
      "cancelled" -> boolean,
      "created" -> longNumber,
      "creator" -> text,
      "modified" -> optional(longNumber),
      "modifier" -> optional(text))(Enrollment.apply)(Enrollment.unapply)
  }

  /**
   * Display a form to create a new enrollment for an {@link Event}.
   *
   * @param id Identifier of the {@link Event} to create the {@link Enrollment} for.
   */
  def create(eid: Long) = isAuthenticated { username =>
    implicit request =>
      val result = Person.getAll
      if (result.isSuccess) {
        val event = Event.load(eid)
        if (event.isDefined) {
          Ok(views.html.enrollmentForm(enrollmentForm.bind(Map("event" -> event.get.id.get.toString)).discardingErrors, result.toOption.get))
        } else {
          Logger.error("Could not find event with ID " + eid)
          Redirect(routes.EventCtrl.show(eid)).flashing("error" -> Messages("error.failedToLoadEvent"))
        }
      } else {
        Logger.error(result.toString(), result.fail.toOption.get)
        Redirect(routes.EventCtrl.show(eid)).flashing("error" -> Messages("error.failedToLoadPersonList"))
      }
  }

  /**
   * Remove an existing {@link Enrollment}.
   *
   * @param id The identifier of the {@link Enrollment} to be removed.
   */
  def delete(id: Long) = isAuthenticated { username =>
    implicit request =>
      val result = Enrollment.delete(id)
      if (result.isSuccess) {
        Logger.debug("Successfully deleted enrollment with ID " + id + ".")
        Redirect(routes.EventCtrl.listUpcoming).flashing(("success" -> Messages("success.succeededToDeleteEnrollment")))
      } else {
        Logger.error(result.toString(), result.fail.toOption.get)
        Redirect(routes.EventCtrl.listUpcoming).flashing(("error" -> Messages("error.failedToDeleteEnrollment")))
      }
  }

  /**
   * Display a form to modify an existing {@link Enrollment}.
   *
   * @param id The identifier of the {@link Enrollment} that is to be loaded into the form.
   */
  def edit(id: Long) = isAuthenticated { username =>
    implicit request =>
      val e = Enrollment.load(id)
      e match {
        case Success(None) =>
          Logger.logger.debug("Cannot find enrollment with ID " + id + "."); NotFound
        case Success(Some(e)) =>
          Logger.logger.debug("Preparing editing of enrollment with ID " + id + ".")
          val result = Person.getAll
          val pList = if (result.isSuccess) result.toOption.get else Nil
          Ok(views.html.enrollmentForm(enrollmentForm.fill(e), pList))
        case _ =>
          Logger.error("Error when attempting to load enrollment with ID " + id + ".", e.fail.toOption.get)
          NotFound
      }
  }

  /**
   * Display the list of current {@link Enrollment}s for an {@link Event}.
   *
   * @param eid The identifier of the {@link Event} to load the list of current {@link Enrollment}s for.
   */
  def listForEvent(eid: Long) = Action { implicit request =>
    val event = Event.load(eid)
    val result = Enrollment.loadByEvent(eid)
    if (result.isSuccess) {
      val req = Ok(views.html.enrollmentsForEventList(result.toOption.get.sortBy(e => e._1.created)))
      if (flash.get("error").isDefined) {
        req.flashing(("error" -> flash.get("error").get))
      } else if (flash.get("success").isDefined) {
        req.flashing(("success" -> flash.get("success").get))
      } else {
        req
      }
    } else {
      Logger.error(result.toString(), result.fail.toOption.get)
      Redirect(routes.EventCtrl.show(eid)).flashing("error" -> Messages("error.failedToLoadEnrollmentsForEventList", event.get.title))
    }
  }

  /**
   * Display a list of {@link Enrollment}s to upcoming {@link Event}s a {@link Person} has.
   *
   * @param pid The identifier of the {@link Person} to load the list of {@link Enrollment}s for.
   */
  def listForPerson(pid: Long) = Action { implicit request =>
    val person = Person.load(pid)
    val result = Enrollment.loadByPerson(pid)
    if (result.isSuccess) {
      val req = Ok(views.html.enrollmentsForPersonList(result.toOption.get.sortBy(e => e._1.created)))
      if (flash.get("error").isDefined) {
        req.flashing(("error" -> flash.get("error").get))
      } else if (flash.get("success").isDefined) {
        req.flashing(("success" -> flash.get("success").get))
      } else {
        req
      }
    } else {
      Logger.error(result.toString(), result.fail.toOption.get)
      Redirect(routes.PersonCtrl.show(pid)).flashing("error" -> Messages("error.failedToLoadEnrollmentsForPersonList", person.get.name))
    }
  }

  /**
   * Handle the form data that was submitted to create a new {@link Enrollment} or modify an existing one.
   */
  def submit = isAuthenticated { username =>
    implicit request =>
      enrollmentForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the enrollment form.")
          for (err <- errors.errors) {
            Logger.error(err.key + " - " + err.message)
          }
          val result = Person.getAll
          if (result.isSuccess) {
            BadRequest(views.html.enrollmentForm(errors, result.toOption.get))
          } else {
            Logger.error(result.toString(), result.fail.toOption.get)
            BadRequest(views.html.enrollmentForm(errors, Nil)).flashing("error" -> Messages("error.failedToLoadPersonList"))
          }
        },
        e => {
          Logger.debug("Storing enrollment " + e)
          val result = Enrollment.saveOrUpdate(e)
          if (result.isSuccess) {
            Redirect(routes.EnrollmentCtrl.listForEvent(result.toOption.get.event)).flashing("success" -> Messages("success.succeededToStoreEnrollment"))
          } else {
            Logger.error(result.toString(), result.fail.toOption.get)
            val res = Person.getAll
            if (res.isSuccess) {
              BadRequest(views.html.enrollmentForm(enrollmentForm, res.toOption.get)).flashing("error" -> Messages("error.failedToStoreEnrollment"))
            } else {
              Logger.error(result.toString(), result.fail.toOption.get)
              BadRequest(views.html.enrollmentForm(enrollmentForm, Nil)).flashing("error" -> Messages("error.failedToLoadPersonList"))
            }
          }
        })
  }
}