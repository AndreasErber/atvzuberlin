/**
 *
 */
package controllers

import controllers.ext.ProvidesCtx
import controllers.ext.Security
import models.{ Event, Events }
import play.api.data.Form
import play.api.data.Forms._
import play.api.db._
import play.api.Logger
import play.api.mvc._
import play.api.mvc.Security._
import play.api.Play.current
import util.CustomFormatters
import util.EventType
import java.sql.Date
import java.sql.Timestamp
import play.api.i18n.Messages
import models.Enrollment
import play.api.templates.Html

/**
 * @author andreas
 * @version 0.0.6, 2015-01-03
 */
object EventCtrl extends Controller with ProvidesCtx with Security {

  implicit val sqlTimestampFormatter = CustomFormatters.sqlTimestampFormatter
  implicit val eventTypeFormatter = CustomFormatters.eventTypeFormatter

  val sqlTimestampMapping = of[Timestamp]
  val eventTypeMapping = of[EventType]

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

  def create = isAuthenticated { username =>
    implicit request =>
      Ok(views.html.eventForm(eventForm.bind(Map("priority" -> "2", "typus" -> "3", "location" -> "Vereinsheim, Onkel-Tom-Straße 52, 14169 Berlin")).discardingErrors))
  }

  def delete(id: Long) = isAuthenticated { username =>
    implicit request =>
      val result = Event.delete(id)
      if (result.isSuccess) {
        Logger.debug("Successfully deleted event with ID " + id + ".")
        Redirect(routes.EventCtrl.listUpcoming).flashing(("success" -> Messages("success.succeededToDeletePerson")))
      } else {
        Logger.error(result.toString(), result.toEither.left.get)
        Redirect(routes.EventCtrl.listUpcoming).flashing(("error" -> Messages("error.failedToDeletePerson")))
      }

  }

  def edit(id: Long) = isAuthenticated { username =>
    implicit request =>
      val e = Event.load(id)
      e match {
        case None =>
          Logger.logger.debug("Cannot find event with ID " + id + "."); NotFound
        case Some(ev) =>
          Logger.logger.debug("Preparing editing of event with ID " + id + ".");
          Ok(views.html.eventForm(eventForm.fill(ev)))
        case _ => NotFound
      }
  }

  def listUpcoming = Action { implicit request =>
    val today = new Timestamp(System.currentTimeMillis())
    val result = Event.getAllUpcoming(today)
    if (result.isSuccess) {
      val req = Ok(views.html.eventList(result.toOption.get.sortBy(e => e.start.getTime())))
      if (request.flash.get("error").isDefined) {
        req.flashing(("error" -> request.flash.get("error").get))
      } else if (request.flash.get("success").isDefined) {
        req.flashing(("success" -> request.flash.get("success").get))
      } else {
        req
      }
    } else {
      Logger.error(result.toString(), result.toEither.left.get)
      Ok(views.html.eventList(List())).flashing("error" -> Messages("error.failedToLoadUpcomingEventsList"))
    }
  }

  def show(id: Long, showEnrs: Boolean = false) = Action { implicit request =>
    val e = Event.load(id)
    e match {
      case None =>
        Logger.logger.debug("No event with ID " + id + " found."); NotFound
      case Some(ev) =>
        Logger.logger.debug("Found event with ID " + id + ".")
        val participants = Enrollment.loadByEvent(id)
        var req = Ok(views.html.event(ev, Nil, None, None))
        if (participants.isSuccess) {
          req = Ok(views.html.event(ev, participants.toOption.get, None, None, showEnrs))
        } 
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
      eventForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the event form.")
          BadRequest(views.html.eventForm(errors))
        },
        event => {
          Logger.debug("Storing event " + event)
          val result = Event.saveOrUpdate(event)
          if (result.isSuccess) {
            Redirect(routes.EventCtrl.show(result.toOption.get.id.get, false)).flashing("success" -> Messages("success.succeededToStoreEvent"))
          } else {
            Logger.error(result.toString(), result.toEither.left.get)
            BadRequest(views.html.eventForm(eventForm)).flashing("error" -> Messages("error.failedToStoreEvent"))
          }
        })
  }
}