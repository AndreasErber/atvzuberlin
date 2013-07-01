package controllers

import play.api._
import play.api.mvc._
import play.api.i18n.Messages
import controllers.ext.ProvidesCtx
import controllers.ext.Security

object Application extends Controller with ProvidesCtx with Security {

  def index = Action { implicit request =>
    Ok(views.html.index("Your new application is ready."))
  }

  def about = Action { implicit request =>
    Ok(views.html.about(Messages("about")))
  }

  def history = Action { implicit request =>
    Ok(views.html.history(Messages("history")))
  }

  def house = Action { implicit request =>
    Ok(views.html.house(Messages("house")))
  }

  def sports = Action { implicit request =>
    Ok(views.html.sports(Messages("sports")))
  }

  def sportsdates = Action { implicit request =>
    Ok(views.html.sportsdates(Messages("sports.dates")))
  }

  def sportsactives = Action { implicit request =>
    Ok(views.html.sportsactives(Messages("sports.actives")))
  }

  def rowing = Action { implicit request =>
    Ok(views.html.rowing(Messages("sports.rowing")))
  }

  def handball = Action { implicit request =>
    Ok(views.html.handball(Messages("sports.handball")))
  }

  def fistball = Action { implicit request =>
    Ok(views.html.fistball(Messages("sports.fistball")))
  }

  def volleyball = Action { implicit request =>
    Ok(views.html.volleyball(Messages("sports.volleyball")))
  }
}