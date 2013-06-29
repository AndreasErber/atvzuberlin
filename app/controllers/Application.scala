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
}