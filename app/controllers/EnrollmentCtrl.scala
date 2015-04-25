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

import scalaz.{Failure, Success}

/**
 * Controller to handle [[Enrollment]]s to [[Event]]s.
 *
 * @author andreas
 * @version 0.0.5, 2015-04-25
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
      val eventV = Event.load(eid)
      if (eventV.isSuccess) {
        val eventOp = eventV.toOption.get
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
          Logger.error(s"Failed to load event with ID '$eid'. Does not exist.")
          Redirect(routes.EventCtrl.listUpcoming()).flashing("error" -> Messages("error.loading.event"))
        }
      } else {
        Logger.error(eventV.toString, eventV.toEither.left.get)
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
          req = routes.EventCtrl.show(enrollmentOp.get.event, showEnrollments)
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
   * @return A response with HTTP status code 200 with the pre-filled form. If the requested [[Enrollment]] cannot be
   *         loaded a redirect to display the list of upcoming [[Event]]s is returned.
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
   * @return A response with HTTP status code 200 with the list of [[Enrollment]]s for the given [[Event]].
   */
  def listForEvent(eid: Long) = Action { implicit request =>
    val eventV = Event.load(eid)
    if (eventV.isSuccess) {
      val eventOp = eventV.toOption.get
      if (eventOp.isDefined) {
        val enrollmentsForEventV = Enrollment.loadByEvent(eid)
        if (enrollmentsForEventV.isSuccess) {
          Ok(views.html.enrollmentsForEventList(eid, enrollmentsForEventV.toOption.get.sortBy(e => e._1.created)))
        } else {
          Logger.error(enrollmentsForEventV.toString, enrollmentsForEventV.toEither.left.get)
          val showEnrollments = false
          Redirect(routes.EventCtrl.show(eid, showEnrollments)).flashing("error" -> Messages("error" +
            ".loading.enrollments.for.event", eventV.toOption.get.get.title))
        }
      } else {
        Logger.error(s"Failed to load event with ID '$eid'. Does not exist.")
        Redirect(routes.EventCtrl.listUpcoming()).flashing("error" -> Messages("error.loading.event"))
      }
    } else {
      Logger.error(eventV.toString, eventV.toEither.left.get)
      Redirect(routes.EventCtrl.listUpcoming()).flashing("error" -> Messages("error.loading.event"))
    }
  }

  /**
   * Load the list of [[Enrollment]]s for the [[Event]] identified by <em>eid</em> but hide the list in display.
   *
   * @param eid Identifier of the [[Event]].
   * @return A response with HTTP status code 200 and the hidden [[Enrollment]]s list for a payload. If the
   *         [[Enrollment]]s cannot be loaded a redirect to display the details of the [[Event]] is returned. If the
   *         [[Event]] cannot be loaded a redirect to display the list of upcomming [[Event]]s is returned.
   */
  def listForEventHide(eid: Long) = Action { implicit request =>
    val eventV = Event.load(eid)
    if (eventV.isSuccess) {
      val eventOp = eventV.toOption.get
      if (eventOp.isDefined) {
        val enrollmentsForEventV = Enrollment.loadByEvent(eid)
        if (enrollmentsForEventV.isSuccess) {
          Ok(views.html.enrollmentsForEventListHide(eid, enrollmentsForEventV.toOption.get.size))
        } else {
          Logger.error(enrollmentsForEventV.toString, enrollmentsForEventV.toEither.left.get)
          val showEnrollments = false
          Redirect(routes.EventCtrl.show(eid, showEnrollments)).flashing("error" -> Messages("error" +
            ".loading.enrollments.for.event", eventOp.get.title))
        }
      } else {
        Logger.error(s"Failed to load event with ID '$eid'. Does not exist.")
        Redirect(routes.EventCtrl.listUpcoming()).flashing("error" -> Messages("error.loading.event"))
      }
    } else {
      Logger.error(eventV.toString, eventV.toEither.left.get)
      Redirect(routes.EventCtrl.listUpcoming()).flashing("error" -> Messages("error.loading.event"))
    }
  }

  /**
   * Display a list of [[Enrollment]]s to upcoming [[Event]]s a [[Person]] has.
   *
   * @param pid The identifier of the [[Person]] to load the list of [[Enrollment]]s for.
   * @return A response with HTTP status code 200 and the list of [[Enrollment]]s for the given [[Person]]. If the
   *         [[Enrollment]]s cannot be loaded a redirect to display the details of the [[Person]] in question is
   *         returned. If the [[Person]] cannot be loaded a redirect to display the list of [[Person]]s is returned.
   */
  def listForPerson(pid: Long) = Action { implicit request =>
    val personV = Person.load(pid)
    personV match {
      case Success(personOp) => personOp match {
        case Some(person) => val enrollmentsForPersonV = Enrollment.loadByPerson(pid)
          enrollmentsForPersonV match {
            case Success(list) => Ok(views.html.enrollmentsForPersonList(list.sortBy(e => e._1.created)))
            case Failure(t) => Logger.error(enrollmentsForPersonV.toString, t)
              Redirect(routes.PersonCtrl.show(pid)).flashing("error" -> Messages("error" +
                ".loading.enrollments.for.person", personOp.get.name))
          }
        case None => Logger.error(s"Failed to load person with ID '$pid'. Does not exist.")
          Redirect(routes.PersonCtrl.list()).flashing("error" -> Messages("error.loading.person"))
      }
      case Failure(t) => Logger.error(personV.toString, t)
        Redirect(routes.PersonCtrl.list()).flashing("error" -> Messages("error.loading.person"))
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
          val eId = errors.data.get("event").get.toLong
          var nonParticipants: List[Person] = Nil
          val personsV = Person.getAll
          personsV match {
            case Success(persons) => nonParticipants = persons
            case Failure(_) =>
          }

          val participantsV = Enrollment.loadByEvent(eId)
          participantsV match {
            case Success(participants) => nonParticipants = removeEnrolledFromPersonList(nonParticipants, participants)
            case Failure(_) =>
          }
          BadRequest(views.html.enrollmentForm(errors, nonParticipants))
        },
        e => {
          Logger.debug("Storing enrollment " + e)
          val enrollmentV = Enrollment.saveOrUpdate(e)
          enrollmentV match {
            case Success(enrollment) => val showEnrollments = true
              Redirect(routes.EventCtrl.show(e.event, showEnrollments)).flashing("success" -> Messages("success" +
                ".storing.enrollment"))
            case Failure(t) => Logger.error(enrollmentV.toString, t)
              val showEnrollments = false
              Redirect(routes.EventCtrl.show(e.event, showEnrollments)).flashing("error" -> Messages("error.storing.enrollment"))
          }
        })
  }

  private def removeEnrolledFromPersonList(pl: List[Person], participants: List[(Enrollment, Person)]): List[Person] =
    for (p <- pl if !participants.unzip._2.contains(p)) yield p
}