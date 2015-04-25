package controllers

import controllers.ext.ProvidesCtx
import models.{Person, PersonInCharges}
import play.api.Logger
import play.api.mvc._

import scalaz.{Failure, Success}

/**
 * Controller to handle requests for the imprint.
 *
 * @author andreas
 * @version 0.0.2, 2015-04-24
 */
object ImprintCtrl extends Controller with ProvidesCtx {

  def show = Action { implicit request =>
    val picsV = PersonInCharges.getAll
    picsV match {
      case Success(pics) => val management = Map[String, Person]()
        for (pic <- pics
          if pic.charge.nameMale.equals("Vorsitz") || pic.charge.nameMale.equals("stellvertretender Vorsitz")
        ) yield pic.charge.nameMale -> pic.person
        val x = if (management.get("Vorsitz").isDefined) management.get("Vorsitz").get.name else "N.N."
          x
        Ok(views.html.imprint(management))
      case Failure(t) => Logger.error(picsV.toString, t)
        NotFound
    }
  }
}
