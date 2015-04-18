/**
 *
 */
package controllers

import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages
import play.api.Logger
import play.api.mvc._
import controllers.ext.ProvidesCtx
import controllers.ext.Security
import models.{Enrollment, Event, Person}
import scalaz.Success
import java.sql.Timestamp

/**
 * Controller to handle [[Enrollment]]s to [[Event]]s.
 *
 * @author andreas
 * @version 0.0.4, 2015-04-18
 */
object EnrollmentCtrl extends Controller with ProvidesCtx with Security {

  /**
   * Form to handle [[Enrollment]]s.
   */
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
   * Display a form to create a new enrollment for an [[Event]].
   *
   * @param eid Identifier of the [[Event]] to create the [[Enrollment]] for.
   * @return A response with HTTP status code 200 holding a prepared form for a payload. If the list of [[Person]]s
   *         cannot be obtained a redirect to display the details of the [[Event]] is returned. If the [[Event]]
   *         cannot be loaded a redirect to display the list of [[Event]]s is returned.
   */
  def create(eid: Long) = isAuthenticated { username =>
    implicit request =>
      val eventOp = Event.load(eid)
      if (eventOp.isDefined) {
        val personsValidation = Person.getAll
        if (personsValidation.isSuccess) {
          val participantsValidation = Enrollment.loadByEvent(eid)
          val persons = if (participantsValidation.isSuccess) {
            removeEnrolledFromPersonList(personsValidation.toOption.get, participantsValidation.toOption.get)
          } else personsValidation.toOption.get
          Ok(views.html.enrollmentForm(enrollmentForm.bind(Map("event" -> eventOp.get.id.get.toString,
            "numberOfAdults" -> "1", "numberOfKids" -> "0")).discardingErrors, persons))
        } else {
          Logger.error(personsValidation.toString, personsValidation.toEither.left.get)
          val showEnrollments = false
          Redirect(routes.EventCtrl.show(eid, showEnrollments)).flashing("error" -> Messages("error.loading.persons"))
        }
      } else {
        Logger.error(s"Failed to load event with ID '$eid'.")
        Redirect(routes.EventCtrl.listUpcoming()).flashing("error" -> Messages("error.loading.event"))
      }
  }

  /**
   * Remove an existing [[Enrollment]].
   *
   * @param id The identifier of the [[Enrollment]] to be removed.
   * @return A redirect to display the details of the [[Event]] the [[Enrollment]] was deleted for. If the [[Event]]
   *         cannot be loaded a redirect to display the list of upcoming [[Event]]s is returned.
   */
  def delete(id: Long) = isAuthenticated { username =>
    implicit request =>
      var req = routes.EventCtrl.listUpcoming()
      val enrollmentV = Enrollment.load(id)
      if (enrollmentV.isSuccess) {
        val enrollmentOp = enrollmentV.toOption.get
        if (enrollmentOp.isDefined) {
          val showEnrollments = true
          req = routes.EventCtrl.show(enrollmentV.toOption.get.get.event, showEnrollments)
        } else {
        }
      }

      val result = Enrollment.delete(id)
      if (result.isSuccess) {
        Logger.debug(s"Successfully deleted enrollment with ID '$id'.")
        Redirect(req).flashing("success" -> Messages("success.deleting.enrollment"))
      } else {
        Logger.error(result.toString, result.toEither.left.get)
        Redirect(req).flashing("error" -> Messages("error.deleting.enrollment"))
      }
  }

  /**
   * Display a form to modify an existing [[Enrollment]].
   *
   * @param id The identifier of the [[Enrollment]] that is to be loaded into the form.
   */
  def edit(id: Long) = isAuthenticated { username =>
    implicit request =>
      val enrollmentV = Enrollment.load(id)
      if (enrollmentV.isSuccess) {
        val enrollmentOp = enrollmentV.toOption.get
        enrollmentOp match {
          case None =>
            Logger.debug(s"Cannot find enrollment with ID '$id'.")
            Redirect(routes.EventCtrl.listUpcoming()).flashing("error" -> Messages("error.loading.enrollment"))
          case Some(e) =>
            Logger.debug(s"Preparing editing of enrollment with ID '$id'.")
            val result = Person.getAll
            val pList = if (result.isSuccess) result.toOption.get else Nil
            Ok(views.html.enrollmentForm(enrollmentForm.fill(e), pList))
          case _ => NotFound
        }
      } else {
        Logger.error(enrollmentV.toString, enrollmentV.toEither.left.get)
        Redirect(routes.EventCtrl.listUpcoming()).flashing("error" -> Messages("error.loading.enrollment"))
      }
  }

  /**
   * Display the list of current [[Enrollment]]s for an [[Event]].
   *
   * @param eid The identifier of the [[Event]] to load the list of current [[Enrollment]]s for.
   */
  def listForEvent(eid: Long) = Action { implicit request =>
    val event = Event.load(eid)
    val result = Enrollment.loadByEvent(eid)
    if (result.isSuccess) {
      val req = Ok(views.html.enrollmentsForEventList(eid, result.toOption.get.sortBy(e => e._1.created)))
      if (request.flash.get("error").isDefined) {
        req.flashing("error" -> request.flash.get("error").get)
      } else if (request.flash.get("success").isDefined) {
        req.flashing("success" -> request.flash.get("success").get)
      } else {
        req
      }
    } else {
      Logger.error(result.toString, result.toEither.left.get)
      val showEnrollments = false
      Redirect(routes.EventCtrl.show(eid, showEnrollments)).flashing("error" -> Messages("error" +
        ".failedToLoadEnrollmentsForEventList", event.get.title))
    }
  }

  def listForEventHide(eid: Long) = Action { implicit request =>
    val result = Enrollment.loadByEvent(eid)
    if (result.isSuccess) {
      Ok(views.html.enrollmentsForEventListHide(eid, result.toOption.get.size))
    } else {
      Logger.error(result.toString, result.toEither.left.get)
      val event = Event.load(eid)
      val title: String = if (event.isDefined) event.get.title else eid.toString
      val showEnrollments = false
      Redirect(routes.EventCtrl.show(eid, showEnrollments)).flashing("error" -> Messages("error" +
        ".failedToLoadEnrollmentsForEventList", title))
    }
  }

  /**
   * Display a list of [[Enrollment]]s to upcoming [[Event]]s a [[Person]] has.
   *
   * @param pid The identifier of the { @link Person} to load the list of [[Enrollment]]s for.
   */
  def listForPerson(pid: Long) = Action { implicit request =>
    val person = Person.load(pid)
    val result = Enrollment.loadByPerson(pid)
    if (result.isSuccess) {
      val req = Ok(views.html.enrollmentsForPersonList(result.toOption.get.sortBy(e => e._1.created)))
      if (request.flash.get("error").isDefined) {
        req.flashing("error" -> request.flash.get("error").get)
      } else if (request.flash.get("success").isDefined) {
        req.flashing("success" -> request.flash.get("success").get)
      } else {
        req
      }
    } else {
      Logger.error(result.toString, result.toEither.left.get)
      Redirect(routes.PersonCtrl.show(pid)).flashing("error" -> Messages("error.failedToLoadEnrollmentsForPersonList", person.get.name))
    }
  }

  /**
   * Handle the form data that was submitted to create a new [[Enrollment]] or modify an existing one.
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
                Logger.error(participants.toString, participants.toEither.left.get)
                Nil
              } else {
                participants.toOption.get
              }
              val pers = if (pl.isFailure) {
                Logger.error(pl.toString, pl.toEither.left.get)
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
            val showEnrollments = true
            Redirect(routes.EventCtrl.show(e.event, showEnrollments)).flashing("success" -> Messages("success" +
              ".succeededToStoreEnrollment"))
          } else {
            Logger.error(result.toString, result.toEither.left.get)
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
                  Logger.error(participants.toString, participants.toEither.left.get)
                  Nil
                } else {
                  participants.toOption.get
                }
                val pers = if (pl.isFailure) {
                  Logger.error(pl.toString, pl.toEither.left.get)
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