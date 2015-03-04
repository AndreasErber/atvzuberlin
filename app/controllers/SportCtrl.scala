/**
 *
 */
package controllers

import controllers.ext.ProvidesCtx
import controllers.ext.Security
import models.Sports
import models.SportsDate
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages
import play.api.Logger
import play.api.mvc._
import play.api.Play.current
import util.CustomFormatters
import models.SportsDates

/**
 * @author andreas
 * @version 0.0.4, 2015-01-03
 */
object SportCtrl extends Controller with ProvidesCtx with Security {

  implicit val sqlTimeFormatter = CustomFormatters.sqlTimeFormatter

  val sqlTimeMapping = of[java.sql.Time]

  val sportsForm = Form[Sports](
    mapping(
      "id" -> optional(longNumber),
      "title" -> nonEmptyText,
      "description" -> optional(text),
      "created" -> longNumber,
      "creator" -> text,
      "modified" -> optional(longNumber),
      "modifier" -> optional(text))(Sports.apply)(Sports.unapply))

  val sportsDatesForm = Form[SportsDate](
    mapping(
      "id" -> optional(longNumber),
      "sports" -> longNumber,
      "locationName" -> optional(text),
      "locationStreet" -> optional(text),
      "locationZip" -> optional(text),
      "locationCity" -> optional(text),
      "weekday" -> optional(text),
      "start" -> optional(sqlTimeMapping),
      "end" -> optional(sqlTimeMapping),
      "created" -> longNumber,
      "creator" -> text,
      "modified" -> optional(longNumber),
      "modifier" -> optional(text))(SportsDate.apply)(SportsDate.unapply))

  def createSports() = isAuthenticated { username =>
    implicit request =>
      Ok(views.html.sportsForm(sportsForm))
  }

  def deleteSports(id: Long) = isAuthenticated { username =>
    implicit request =>
      val result = Sports.delete(id)
      if (result.isSuccess) {
        Logger.debug("Successfully deleted sports with ID " + id + ".")
        Redirect(routes.SportCtrl.listSports).flashing(("success" -> Messages("success.succeededToDeleteSports")))
      } else {
        Logger.error(result.toString(), result.toEither.left.get)
        Redirect(routes.SportCtrl.listSports).flashing(("error" -> Messages("error.failedToDeleteSports")))
      }
  }

  def editSports(id: Long) = isAuthenticated { username =>
    implicit request =>
      val n = Sports.load(id)
      n match {
        case None =>
          Logger.logger.debug("Cannot find sports with ID " + id + "."); NotFound
        case Some(sports) =>
          Logger.logger.debug("Preparing editing of sports with ID " + id + ".");
          Ok(views.html.sportsForm(sportsForm.fill(sports)))
        case _ => NotFound
      }
  }

  def listSports() = Action { implicit request =>

    val result = Sports.getAll()
    if (result.isSuccess) {
      val req = Ok(views.html.sports(result.toOption.get))
      if (request.flash.get("error").isDefined) {
        req.flashing(("error" -> request.flash.get("error").get))
      } else if (request.flash.get("success").isDefined) {
        req.flashing(("success" -> request.flash.get("success").get))
      } else {
        req
      }
    } else {
      Logger.error(result.toString(), result.toEither.left.get)
      Ok(views.html.sports(List())).flashing("error" -> Messages("error.failedToLoadSportsList"))
    }
  }

  def showSports(id: Long) = Action { implicit request =>
    val s = Sports.load(id)
    s match {
      case None =>
        Logger.logger.debug("No sports with ID " + id + " found."); NotFound
      case Some(ev) =>
        Logger.logger.debug("Found sports with ID " + id + ".")
        val req = Ok(views.html.sports(List(ev)))
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

  def submitSports() = isAuthenticated { username =>
    implicit request =>
      sportsForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the sports form.")
          BadRequest(views.html.sportsForm(errors))
        },
        sports => {
          Logger.debug("Storing sports item " + sports)
          val result = Sports.saveOrUpdate(sports)
          if (result.isSuccess) {
            Redirect(routes.SportCtrl.showSports(result.toOption.get.id.get)).flashing("success" -> Messages("success.succeededToStoreSports"))
          } else {
            Logger.error(result.toString(), result.toEither.left.get)
            BadRequest(views.html.sportsForm(sportsForm)).flashing("error" -> Messages("error.failedToStoreSports"))
          }
        })
  }

  /* Sportsdates is done here */
  def createSportsDate() = isAuthenticated { username =>
    implicit request =>
      val sports = Sports.getAll
      if (sports.isSuccess) {
        Ok(views.html.sportsDateForm(sportsDatesForm, sports.toOption.get))
      } else {
        Ok(views.html.sportsDateForm(sportsDatesForm.withGlobalError(Messages("error.failedToLoadSportsList")), List()))
      }
  }

  def deleteSportsDate(id: Long) = isAuthenticated { username =>
    implicit request =>
      val result = SportsDate.delete(id)
      if (result.isSuccess) {
        Logger.debug("Successfully deleted sports date with ID " + id + ".")
        Redirect(routes.SportCtrl.listSports).flashing(("success" -> Messages("success.succeededToDeleteSportsDate")))
      } else {
        Logger.error(result.toString(), result.toEither.left.get)
        Redirect(routes.SportCtrl.listSports).flashing(("error" -> Messages("error.failedToDeleteSportsDate")))
      }
  }

  def editSportsDate(id: Long) = isAuthenticated { username =>
    implicit request =>
      val sports = Sports.getAll
      val sd = SportsDate.load(id)
      sd match {
        case None =>
          Logger.logger.debug("Cannot find sports date with ID " + id + "."); NotFound
        case Some(sportsDate) =>
          Logger.logger.debug("Preparing editing of sports date with ID " + id + ".");
          if (sports.isSuccess) {
            Ok(views.html.sportsDateForm(sportsDatesForm.fill(sportsDate).withError("sports", Messages("error.failedToLoadSportsList")), List()))
          } else {
            Ok(views.html.sportsDateForm(sportsDatesForm.fill(sportsDate), List()))
          }
        case _ => NotFound
      }
  }

  def listSportsDates() = Action { implicit request =>

    val result = SportsDate.getAll()
    if (result.isSuccess) {
      val req = Ok(views.html.sportsdates(result.toOption.get))
      if (request.flash.get("error").isDefined) {
        req.flashing(("error" -> request.flash.get("error").get))
      } else if (request.flash.get("success").isDefined) {
        req.flashing(("success" -> request.flash.get("success").get))
      } else {
        req
      }
    } else {
      Logger.error(result.toString(), result.toEither.left.get)
      Ok(views.html.sports(List())).flashing("error" -> Messages("error.failedToLoadSportsDateList"))
    }
  }

  def submitSportsDate() = isAuthenticated { username =>
    implicit request =>
      sportsDatesForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the sports date form.")
          for (err <- errors.errors) {
            Logger.error(err.key + " - " + err.message)
          }
          for (datum <- errors.data) {
            Logger.debug(datum._1 + " - " + datum._2)
          }
          val sports = Sports.getAll
          if (sports.isSuccess)
            BadRequest(views.html.sportsDateForm(errors, sports.toOption.get))
          else
            BadRequest(views.html.sportsDateForm(errors.withError("sports", Messages("error.failedToLoadSportsList")), List()))
        },
        sportsDate => {
          Logger.debug("Storing sports date item " + sportsDate)
          val result = SportsDate.saveOrUpdate(sportsDate)
          if (result.isSuccess) {
            Redirect(routes.SportCtrl.listSportsDates()).flashing("success" -> Messages("success.succeededToStoreSportsDate"))
          } else {
            Logger.error(result.toString(), result.toEither.left.get)
            val sports = Sports.getAll
            if (sports.isSuccess)
              BadRequest(views.html.sportsDateForm(sportsDatesForm, sports.toOption.get)).flashing("error" -> Messages("error.failedToStoreSportsDate"))
            else
              BadRequest(views.html.sportsDateForm(sportsDatesForm.withError("sports", Messages("error.failedToLoadSportsList")), List())).flashing("error" -> Messages("error.failedToStoreSportsDate"))
          }
        })
  }

}