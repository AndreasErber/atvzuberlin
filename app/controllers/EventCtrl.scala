/**
 *
 */
package controllers

import controllers.ext.{ProvidesCtx, Security}
import models.{Enrollment, Event}
import play.api.data.Form
import play.api.data.Forms._
import play.api.Logger
import play.api.mvc._
import util.{CustomFormatters, EventType}
import java.sql.Timestamp
import play.api.i18n.Messages

/**
 * Controller to handle requests on [[Event]]s.
 *
 * @author andreas
 * @version 0.0.7, 2015-04-18
 */
object EventCtrl extends Controller with ProvidesCtx with Security {

  implicit val sqlTimestampFormatter = CustomFormatters.sqlTimestampFormatter
  implicit val eventTypeFormatter = CustomFormatters.eventTypeFormatter

  val sqlTimestampMapping = of[Timestamp]
  val eventTypeMapping = of[EventType]

  /**
   * Form to display, enter, and modify events.
   */
  val eventForm = Form(
    mapping("id" -> optional(longNumber),
      "title" -> nonEmptyText,
      "description" -> optional(text),
      "start" -> sqlTimestampMapping,
      "end" -> optional(sqlTimestampMapping),
      "location" -> optional(text),
      "url" -> optional(text),
      "priority" -> number,
      "typus" -> eventTypeMapping,
      "created" -> longNumber,
      "creator" -> text,
      "modified" -> optional(longNumber),
      "modifier" -> optional(text))(Event.apply)(Event.unapply))

  /**
   * Display an empty form to enter an event.
   *
   * @return A response with HTTP status code 200 and an empty form.
   */
  def create = isAuthorized("create.event") { username =>
    implicit request =>
      Ok(views.html.eventForm(eventForm.bind(Map("priority" -> "2", "typus" -> "3", "location" -> "Vereinsheim, Onkel-Tom-Straße 52, 14169 Berlin")).discardingErrors))
  }

  /**
   * Delete the [[Event]] identified by the given <em>id</em>
   *
   * @param id The identifier of the [[Event]] to delete.
   * @return A redirect to the list of upcoming [[Event]]s flashing either success or error.
   */
  def delete(id: Long) = isAuthorized("delete.event") { username =>
    implicit request =>
      val result = Event.delete(id)
      if (result.isSuccess) {
        Logger.debug(s"Successfully deleted event with ID $id.")
        Redirect(routes.EventCtrl.listUpcoming()).flashing("success" -> Messages("success.deleting.event"))
      } else {
        Logger.error(result.toString, result.toEither.left.get)
        Redirect(routes.EventCtrl.listUpcoming()).flashing("error" -> Messages("error.deleting.event"))
      }
  }

  /**
   * Load the [[Event]] identified by the given <em>id</em> into the form for editing.
   *
   * @param id The identifier of the [[Event]] to load for editing.
   * @return A response with HTTP status code 200 and the [[Event]] for a payload. In case of error
   *         a redirect to the list of upcoming [[Event]]s is returned.
   */
  def edit(id: Long) = isAuthorized("edit.event") { username =>
    implicit request =>
      val e = Event.load(id)
      e match {
        case None =>
          Logger.debug(s"Cannot find event with ID $id.")
          Redirect(routes.EventCtrl.listUpcoming()).flashing("error" -> Messages("error.loading.event"))
        case Some(ev) =>
          Logger.debug(s"Preparing editing of event with ID $id.")
          Ok(views.html.eventForm(eventForm.fill(ev)))
        case _ => NotFound
      }
  }

  /**
   * List the upcoming [[Event]]s.
   *
   * @return A response with HTTP status code 200 and the list of [[Event]]s for a payload. In case
   *         of error, a response with status code 400.
   */
  def listUpcoming = Action { implicit request =>
    val today = new Timestamp(System.currentTimeMillis())
    val result = Event.getAllUpcoming(today)
    if (result.isSuccess) {
      Ok(views.html.eventList(result.toOption.get.sortBy(e => e.start.getTime)))
    } else {
      Logger.error(result.toString, result.toEither.left.get)
      BadRequest(Messages("error.loading.upcoming.events"))
    }
  }

  /**
   * Display the [[Event]] identified by the given <em>id</em>
   *
   * @param id Identifier of the [[Event]] to display.
   * @param showEnrollments Flag to indicate if the [[Enrollment]]s are to be loaded as well.
   * @return An HTTP response with status code 200 and the details of the [[Event]] found by the
   *         <em>id</em>. In case of error a redirect to the list of [[Event]]s is returned.
   */
  def show(id: Long, showEnrollments: Boolean = false) = Action { implicit request =>
    val e = Event.load(id)
    e match {
      case None =>
        Logger.debug(s"No event with ID $id found.")
        Redirect(routes.EventCtrl.listUpcoming()).flashing("error" -> Messages("error.loading.event"))
      case Some(ev) =>
        Logger.debug(s"Found event with ID $id.")
        val participants = Enrollment.loadByEvent(id)
        var req = Ok(views.html.event(ev, Nil, None, None))
        if (participants.isSuccess) {
          req = Ok(views.html.event(ev, participants.toOption.get, None, None, showEnrollments))
        } else {
          Logger.error(s"Failed to load enrollments for event $id.")
        }
        req
      case _ => NotFound
    }
  }

  /**
   * Store the submitted data.
   *
   * @return A redirect to display the details of the newly created [[Event]], in case of error a
   *         [[BadRequest]] displaying the error.
   */
  def submit = isAuthorized("save.event") { username =>
    implicit request =>
      eventForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the event form.")
          BadRequest(views.html.eventForm(errors))
        },
        event => {
          Logger.debug(s"Storing event $event.")
          val result = Event.saveOrUpdate(event)
          if (result.isSuccess) {
            val showEnrollments = false
            Redirect(routes.EventCtrl.show(result.toOption.get.id.get, showEnrollments)).flashing("success" -> Messages("success.storing.event"))
          } else {
            Logger.error(result.toString, result.toEither.left.get)
            BadRequest(views.html.eventForm(eventForm)).flashing("error" -> Messages("error.storing.event"))
          }
        })
  }
}