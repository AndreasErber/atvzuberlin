package controllers

import play.api._
import play.api.mvc._
import controllers.ext.ProvidesCtx
import controllers.ext.Security

object Application extends Controller with ProvidesCtx with Security {

  def index = Action { implicit request =>
    Ok(views.html.index("Your new application is ready."))
  }
}