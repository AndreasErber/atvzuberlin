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
import models.{ AcademicTitle, AcademicTitles, PersonHasTitle, PersonHasTitles, Person }
import util.CustomFormatters

/**
 * @author andreas
 * @version 0.0.2, 2013-04-21
 */
object AcademicTitleCtrl extends Controller with ProvidesCtx with Security {

  val titleForm = Form[AcademicTitle] {
    mapping(
      "id" -> optional(longNumber),
      "abbr" -> nonEmptyText,
      "maleForm" -> optional(text),
      "femaleForm" -> optional(text),
      "created" -> longNumber,
      "creator" -> text,
      "modified" -> optional(longNumber),
      "modifier" -> optional(text))(AcademicTitle.apply)(AcademicTitle.unapply)
  }
  
  val personTitleForm = Form[(Person, AcademicTitle)] {
    mapping(
        )
  }

  def create = isAuthenticated { username =>
    implicit request =>
      Ok(views.html.academicTitleForm(titleForm))
  }
  
  def delete(id: Long) = isAuthenticated { username =>
    implicit request =>
      val result = AcademicTitle.delete(id)
      if (result.isSuccess) {
        Logger.debug("Successfully deleted academic title with ID " + id + ".")
        Redirect(routes.AcademicTitleCtrl.list).flashing(("success" -> Messages("success.succeededToDeleteAcademicTitle")))
      } else {
        Logger.error(result.toString(), result.fail.toOption.get)
        Redirect(routes.AcademicTitleCtrl.list).flashing(("error" -> Messages("error.failedToDeleteAcademicTitle")))
      }
  }

  def edit(id: Long) = isAuthenticated { username =>
    implicit request =>
      val e = AcademicTitle.load(id)
      e match {
        case None =>
          Logger.logger.debug("Cannot find academic title with ID " + id + "."); NotFound
        case Some(ev) =>
          Logger.logger.debug("Preparing editing of academic title with ID " + id + ".");
          Ok(views.html.academicTitleForm(titleForm.fill(ev)))
        case _ => NotFound
      }
  }
  
  def list = Action { implicit request =>
    val result = AcademicTitle.getAll
    if (result.isSuccess) {
      val req = Ok(views.html.academicTitleList(result.toOption.get.sortBy(at => at.id)))
      if (flash.get("error").isDefined) {
        req.flashing(("error" -> flash.get("error").get))
      } else if (flash.get("success").isDefined) {
        req.flashing(("success" -> flash.get("success").get))
      } else {
        req
      }
    } else {
      Logger.error(result.toString(), result.fail.toOption.get)
      Ok(views.html.academicTitleList(List())).flashing("error" -> Messages("error.failedToLoadAcademicTitlesList"))
    }
  }
  
  def show(id: Long) = Action { implicit request =>
    val result = AcademicTitle.load(id)
    result match {
      case None =>
        Logger.logger.debug("No academic title with ID " + id + " found."); NotFound
      case Some(at) =>
        Logger.logger.debug("Found academic title with ID " + id + ".")
        val req = Ok(views.html.academicTitle(at))
        if (flash.get("error").isDefined) {
        req.flashing(("error" -> flash.get("error").get))
      } else if (flash.get("success").isDefined) {
        req.flashing(("success" -> flash.get("success").get))
      } else {
        req
      }
      case _ => NotFound
    }
  }

  def submit = isAuthenticated { username =>
    implicit request =>
      titleForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the academic title form.")
          BadRequest(views.html.academicTitleForm(errors))
        },
        at => {
          Logger.debug("Storing academic title " + at)
          val result = AcademicTitle.saveOrUpdate(at)
          if (result.isSuccess) {
            Redirect(routes.AcademicTitleCtrl.show(result.toOption.get.id.get)).flashing("success" -> Messages("success.succeededToStoreAcademicTitle"))
          } else {
            Logger.error(result.toString(), result.fail.toOption.get)
            BadRequest(views.html.academicTitleForm(titleForm)).flashing("error"-> Messages("error.failedToStoreAcademicTitle"))
          }
        })
  }
}