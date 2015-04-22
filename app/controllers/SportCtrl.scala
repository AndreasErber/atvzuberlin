/**
 *
 */
package controllers

import controllers.ext.{ProvidesCtx, Security}
import models.{Sports, SportsDate}
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages
import play.api.Logger
import play.api.mvc._
import util.CustomFormatters

import scalaz.{Failure, Success}

/**
 * Controller to handle requests on sports.
 *
 * @author andreas
 * @version 0.0.6, 2015-04-21
 */
object SportCtrl extends Controller with ProvidesCtx with Security {

  implicit val sqlTimeFormatter = CustomFormatters.sqlTimeFormatter

  val sqlTimeMapping = of[java.sql.Time]

  /**
   * A form for sports.
   */
  val sportsForm = Form[Sports](
    mapping(
      "id" -> optional(longNumber),
      "title" -> nonEmptyText,
      "description" -> optional(text),
      "created" -> longNumber,
      "creator" -> text,
      "modified" -> optional(longNumber),
      "modifier" -> optional(text))(Sports.apply)(Sports.unapply))

  /**
   * A form for sports dates.
   */
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

  /**
   * Display an empty form for [[Sports]].
   *
   * @return A response with HTTP status code 200 and an empty form for a payload
   */
  def createSports() = isAuthenticated { username =>
    implicit request =>
      Ok(views.html.sportsForm(sportsForm))
  }

  /**
   * Delete the [[Sports]] identified by the <em>id</em>.
   *
   * @param id Identifier of the [[Sports]] to delete.
   * @return
   */
  def deleteSports(id: Long) = isAuthenticated { username =>
    implicit request =>
      val result = Sports.delete(id)
      if (result.isSuccess) {
        Logger.debug("Successfully deleted sports with ID " + id + ".")
        Redirect(routes.SportCtrl.listSports()).flashing("success" -> Messages("success.succeededToDeleteSports"))
      } else {
        Logger.error(result.toString, result.toEither.left.get)
        Redirect(routes.SportCtrl.listSports()).flashing("error" -> Messages("error.failedToDeleteSports"))
      }
  }

  def editSports(id: Long) = isAuthenticated { username =>
    implicit request =>
      val sportsV = Sports.load(id)
      sportsV match {
        case Success(sportsOp) =>
          sportsOp match {
            case Some(sports) => Ok(views.html.sportsForm(sportsForm.fill(sports)))
            case None =>
              Logger.error(s"Failed to load sports with ID '$id'. Does not exist.")
              Redirect(routes.SportCtrl.listSports()).flashing("error" -> Messages("error.loading.sports"))
          }
        case Failure(t) => Logger.error(sportsV.toString, t)
          Redirect(routes.SportCtrl.listSports()).flashing("error" -> Messages("error.loading.sports"))
      }
  }

  /**
   * Display a list of all sports offered.
   *
   * @return
   */
  def listSports = Action { implicit request =>
    val sportsV = Sports.getAll
    sportsV match {
      case Success(sports) => Ok(views.html.sports(sports))
      case Failure(t) => Logger.error(sportsV.toString, t)
        Ok(views.html.sports(List())).flashing("error" -> Messages("error.loading.sports"))
    }
  }

  /**
   * Display a single sports.
   *
   * @param id Identifier of the sports to display.
   * @return
   */
  def showSports(id: Long) = Action {
    implicit request =>
      val sportV = Sports.load(id)
      sportV match {
        case Success(sportOp) =>
          sportOp match {
            case Some(sport) => Ok(views.html.sports(List(sport)))
            case None => Logger.debug(s"Failed to load sports with ID '$id'. Does not exist.")
              Redirect(routes.SportCtrl.listSports()).flashing("error" -> Messages("error.loading.sports"))
          }
        case Failure(t) => Logger.error(sportV.toString, t)
          Redirect(routes.SportCtrl.listSports()).flashing("error" -> Messages("error.loading.sports"))
      }
  }

  /**
   *
   * @return
   */
  def submitSports() = isAuthenticated {
    username =>
      implicit request =>
        sportsForm.bindFromRequest.fold(
          errors => {
            Logger.error("An error occurred when trying to process the sports form.")
            BadRequest(views.html.sportsForm(errors))
          },
          sports => {
            val resultV = Sports.saveOrUpdate(sports)
            resultV match {
              case Success(result) => Redirect(routes.SportCtrl.showSports(result.id.get)).flashing("success" ->
                Messages("success.storing.sports"))
              case Failure(t) => Logger.error(resultV.toString, t)
                BadRequest(views.html.sportsForm(sportsForm)).flashing("error" -> Messages("error.storing.sports"))
            }
          }
        )
  }

  /* Sportsdates is done here */

  /**
   * Display an empty form for [[SportsDate]].
   * @return
   */
  def createSportsDate() = isAuthenticated {
    username =>
      implicit request =>
        val sportsV = Sports.getAll
        sportsV match {
          case Success(sportsDate) =>
            Ok(views.html.sportsDateForm(sportsDatesForm, sportsDate))
          case Failure(t) =>
            Ok(views.html.sportsDateForm(sportsDatesForm.withGlobalError(Messages("error.loading.sport.dates")), List
              ()))
        }
  }

  def deleteSportsDate(id: Long) = isAuthenticated {
    username =>
      implicit request =>
        val result = SportsDate.delete(id)
        if (result.isSuccess) {
          Logger.debug("Successfully deleted sports date with ID " + id + ".")
          Redirect(routes.SportCtrl.listSports()).flashing("success" -> Messages("success.succeededToDeleteSportsDate"))
        } else {
          Logger.error(result.toString, result.toEither.left.get)
          Redirect(routes.SportCtrl.listSports()).flashing("error" -> Messages("error.failedToDeleteSportsDate"))
        }
  }

  def editSportsDate(id: Long) = isAuthenticated {
    username =>
      implicit request =>
        val sportsV = Sports.getAll
        sportsV match {
          case Success(sports) => val sportDatesV = SportsDate.load(id)
            sportDatesV match {
              case Success(sd) => sd match {
                case Some(sportsDate) =>
                  Ok(views.html.sportsDateForm(sportsDatesForm.fill(sportsDate), sports))
                case None =>
                  Logger.logger.debug("Cannot find sports date with ID " + id + ".")
                  Redirect(routes.SportCtrl.listSportsDates()).flashing("error" -> Messages("error.loading.sports.date"))
              }
              case Failure(t) => Logger.error(sportDatesV.toString, t)
                Redirect(routes.SportCtrl.listSportsDates()).flashing("error" -> Messages("error.loading.sports.date"))
            }
          case Failure(t) => Redirect(routes.SportCtrl.listSports()).flashing("error" -> Messages("error.loading" +
            ".sports"))
        }
  }

  def listSportsDates = Action {
    implicit request =>
      val sportsDatesV = SportsDate.getAll
      sportsDatesV match {
        case Success(sportsDates) => val sportsV = Sports.getAll
          sportsV match {
            case Success(sports) => val pairList = createSportsDateSportsPairList(sportsDates, sports)
              val groupedList = pairList.groupBy(s => s._1)
              Ok(views.html.sportsdates(groupedList))
            case Failure(t) => Logger.error(sportsV.toString, t)
              Ok(views.html.sports(List())).flashing("error" -> Messages("error.loading.sports"))
          }

        case Failure(t) => Logger.error(sportsDatesV.toString, t)
          Ok(views.html.sports(List())).flashing("error" -> Messages("error.loading.sports.dates"))
      }
  }

  def submitSportsDate() = isAuthenticated {
    username =>
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
              Logger.error(result.toString, result.toEither.left.get)
              val sports = Sports.getAll
              if (sports.isSuccess)
                BadRequest(views.html.sportsDateForm(sportsDatesForm, sports.toOption.get)).flashing("error" -> Messages("error.failedToStoreSportsDate"))
              else
                BadRequest(views.html.sportsDateForm(sportsDatesForm.withError("sports", Messages("error.failedToLoadSportsList")), List())).flashing("error" -> Messages("error.failedToStoreSportsDate"))
            }
          })
  }

  /**
   * Create a list of matching pairs of [[SportsDate]] and [[Sports]].
   *
   * @param sportsDates The time and location of a sports.
   * @param sports The sports offered.
   * @return
   */
  private def createSportsDateSportsPairList(sportsDates: List[SportsDate], sports: List[Sports]): List[
    (String, SportsDate)] = {
    for {
      sportsDate <- sportsDates
      sport <- sports
      if sportsDate.sports == sport.id.get
    } yield sport.title -> sportsDate
  }
}