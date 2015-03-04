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
import java.sql.Timestamp

/**
 * @author andreas
 * @version 0.0.3, 2015-01-03
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
          val participants = Enrollment.loadByEvent(eid)
          val pers = if (participants.isSuccess) {
            removeEnrolledFromPersonList(result.toOption.get, participants.toOption.get)
          } else result.toOption.get
          Ok(views.html.enrollmentForm(enrollmentForm.bind(Map("event" -> event.get.id.get.toString, "numberOfAdults" -> "1", "numberOfKids" -> "0")).discardingErrors, pers))
        } else {
          Logger.error("Could not find event with ID " + eid)
          Redirect(routes.EventCtrl.listUpcoming).flashing("error" -> Messages("error.failedToLoadEvent"))
        }
      } else {
        Logger.error(result.toString(), result.toEither.left.get)
        Redirect(routes.EventCtrl.show(eid, false)).flashing("error" -> Messages("error.failedToLoadPersonList"))
      }
  }

  /**
   * Remove an existing {@link Enrollment}.
   *
   * @param id The identifier of the {@link Enrollment} to be removed.
   */
  def delete(id: Long) = isAuthenticated { username =>
    implicit request =>
      val enr = Enrollment.load(id)
      var req = routes.EventCtrl.listUpcoming
      if (enr.isSuccess){
        req = routes.EventCtrl.show(enr.toOption.get.get.event, true)
      }
      val result = Enrollment.delete(id)
      if (result.isSuccess) {
        Logger.debug("Successfully deleted enrollment with ID " + id + ".")
        Redirect(req).flashing(("success" -> Messages("success.succeededToDeleteEnrollment")))
      } else {
        Logger.error(result.toString(), result.toEither.left.get)
        Redirect(req).flashing(("error" -> Messages("error.failedToDeleteEnrollment")))
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
          Logger.error("Error when attempting to load enrollment with ID " + id + ".", e.toEither.left.get)
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
      val req = Ok(views.html.enrollmentsForEventList(eid, result.toOption.get.sortBy(e => e._1.created)))
      if (request.flash.get("error").isDefined) {
        req.flashing(("error" -> request.flash.get("error").get))
      } else if (request.flash.get("success").isDefined) {
        req.flashing(("success" -> request.flash.get("success").get))
      } else {
        req
      }
    } else {
      Logger.error(result.toString(), result.toEither.left.get)
      Redirect(routes.EventCtrl.show(eid, false)).flashing("error" -> Messages("error.failedToLoadEnrollmentsForEventList", event.get.title))
    }
  }
  
  def listForEventHide(eid: Long) = Action { implicit request =>
    val result = Enrollment.loadByEvent(eid)
    if (result.isSuccess) {
      Ok(views.html.enrollmentsForEventListHide(eid, result.toOption.get.size))
    } else {
      Logger.error(result.toString(), result.toEither.left.get)
      val event = Event.load(eid)
      val title: String = if (event.isDefined) event.get.title else eid.toString
      Redirect(routes.EventCtrl.show(eid, false)).flashing("error" -> Messages("error.failedToLoadEnrollmentsForEventList", title))
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
      if (request.flash.get("error").isDefined) {
        req.flashing(("error" -> request.flash.get("error").get))
      } else if (request.flash.get("success").isDefined) {
        req.flashing(("success" -> request.flash.get("success").get))
      } else {
        req
      }
    } else {
      Logger.error(result.toString(), result.toEither.left.get)
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
          val evId = errors.data.get("event").get.toInt
          val ev = Event.load(evId)
          val participants = Enrollment.loadByEvent(evId)
          val pl = Person.getAll
          if (ev.isDefined && participants.isSuccess && pl.isSuccess) {
            val pers = Some(removeEnrolledFromPersonList(pl.toOption.get, participants.toOption.get))
            BadRequest(views.html.event(ev.get, participants.toOption.get, Some(errors), pl.toOption))
          } else {
            if (!ev.isDefined) {
              Logger.error("Failed to load event with ID " + evId + ".")
              val el = Event.getAllUpcoming(new Timestamp(System.currentTimeMillis()))
              if (el.isSuccess) {
                BadRequest(views.html.eventList(el.toOption.get)).flashing("error" -> Messages("error.failedToLoadEvent", evId))
              } else {
                BadRequest(views.html.index("")).flashing("error" -> Messages("error.failedToLoadEvent", evId))
              }
            } else {
              val parts = if (participants.isFailure) {
                Logger.error(participants.toString(), participants.toEither.left.get)
                Nil
              } else {
                participants.toOption.get
              }
              val pers = if (pl.isFailure) {
                Logger.error(pl.toString(), pl.toEither.left.get)
                None
              } else {
                // remove all participants that are already enrolled
                Some(removeEnrolledFromPersonList(pl.toOption.get, parts))
              }
              BadRequest(views.html.event(ev.get, parts, Some(errors), pers)).flashing("error" -> Messages("error.failedToLoadPersonList"))
            }

          }
        },
        e => {
          Logger.debug("Storing enrollment " + e)
          val result = Enrollment.saveOrUpdate(e)
          if (result.isSuccess) {
            Redirect(routes.EventCtrl.show(e.event, true)).flashing("success" -> Messages("success.succeededToStoreEnrollment"))
          } else {
            Logger.error(result.toString(), result.toEither.left.get)
            val ev = Event.load(e.event)
            val participants = Enrollment.loadByEvent(e.event)
            val pl = Person.getAll
            if (ev.isDefined && participants.isSuccess && pl.isSuccess) {
              val pers = Some(removeEnrolledFromPersonList(pl.toOption.get, participants.toOption.get))
              BadRequest(views.html.event(ev.get, participants.toOption.get, Some(enrollmentForm), pers)).flashing("error" -> Messages("error.failedToStoreEnrollment"))
            } else {
              if (!ev.isDefined) {
                Logger.error("Failed to load event with ID " + e.event + ".")
                val el = Event.getAllUpcoming(new Timestamp(System.currentTimeMillis()))
                if (el.isSuccess) {
                  BadRequest(views.html.eventList(el.toOption.get)).flashing("error" -> Messages("error.failedToLoadEvent", e.event))
                } else {
                  BadRequest(views.html.index("")).flashing("error" -> Messages("error.failedToLoadEvent", e.event))
                }
              } else {
                val parts = if (participants.isFailure) {
                  Logger.error(participants.toString(), participants.toEither.left.get)
                  Nil
                } else {
                  participants.toOption.get
                }
                val pers = if (pl.isFailure) {
                  Logger.error(pl.toString(), pl.toEither.left.get)
                  None
                } else {
                  // remove all participants that are already enrolled
                  Some(removeEnrolledFromPersonList(pl.toOption.get, parts))
                }
                BadRequest(views.html.event(ev.get, parts, Some(enrollmentForm.fill(e)), pers)).flashing("error" -> Messages("error.failedToLoadPersonList"))
              }
            }
          }
        })
  }

  private def removeEnrolledFromPersonList(pl: List[Person], participants: List[(Enrollment, Person)]): List[Person] = 
    for (p <- pl if !participants.unzip._2.contains(p)) yield p
}