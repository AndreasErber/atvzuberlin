package controllers

import controllers.ext.ProvidesCtx
import models.{AcademicTitle, PersonInCharges}
import play.api.Logger
import play.api.i18n.Messages
import play.api.mvc._


import scala.collection.mutable.ListBuffer
import scalaz.{Failure, Success}

/**
 * Controller to handle requests for the management.
 *
 * @author andreas
 * @version 0.0.2, 2015-04-25
 */
object ManagementCtrl extends Controller with ProvidesCtx {

  def display = Action { implicit request =>
    val picsV = PersonInCharges.getAllCurrent
    picsV match {
      case Success(pics) => val picsSorted = pics.sortWith(_.charge.position < _.charge.position)
        var acadTitles = new ListBuffer[List[AcademicTitle]]
        for (p <- picsSorted.map(pic => pic.person)) {
          val personTitlesV = AcademicTitle.getPersonTitles(p)
          personTitlesV match {
            case Success(personTitles) => acadTitles += personTitles
            case Failure(t) => Logger.error(personTitlesV.toString, t)
              acadTitles += Nil
          }
        }
        val map = (picsSorted zip acadTitles).toMap
        Ok(views.html.management(map))
      case Failure(t) => Logger.error(picsV.toString, t)
        Redirect(routes.UserCtrl.index()).flashing("error" -> Messages("error.loading.person.in.charges"))
    }
  }
}
