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
    Ok(views.html.staticpages.about(Messages("about")))
  }

  def administration = isAuthenticated { username =>
    implicit request =>
      Ok(views.html.staticpages.administration(Messages("administration")))
  }

  def history = Action { implicit request =>
    Ok(views.html.staticpages.history(Messages("history")))
  }

  def house = Action { implicit request =>
    Ok(views.html.staticpages.house(Messages("house")))
  }

  def sportsactives = Action { implicit request =>
    Ok(views.html.staticpages.sportsactives(Messages("sports.actives")))
  }

  def rowing = Action { implicit request =>
    Ok(views.html.staticpages.rowing(Messages("sports.rowing")))
  }

  def handball = Action { implicit request =>
    Ok(views.html.staticpages.handball(Messages("sports.handball")))
  }

  def fistball = Action { implicit request =>
    Ok(views.html.staticpages.fistball(Messages("sports.fistball")))
  }

  def volleyball = Action { implicit request =>
    Ok(views.html.staticpages.volleyball(Messages("sports.volleyball")))
  }
}