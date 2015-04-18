package controllers

import play.api.mvc._
import play.api.i18n.Messages
import controllers.ext.{ProvidesCtx, Security}

/**
 * Basic application controller.
 *
 * Serves static pages.
 *
 * @author andreas
 * @version 0.0.5, 2015-04-18
 */
object Application extends Controller with ProvidesCtx with Security {

  /**
   * Display the index page.
   *
   * @return A response with HTTP status code 200 and the static index page.
   */
  def index = Action { implicit request =>
    Ok(views.html.index("Your new application is ready."))
  }

  /**
   * Display the about page
   *
   * @return A response with HTTP status code 200 and the static about page.
   */
  def about = Action { implicit request =>
    Ok(views.html.staticpages.about(Messages("about")))
  }

  /**
   * Display the administration overview page.
   *
   * @return A response with HTTP status code 200 and the static administration overview page.
   */
  def administration = isAuthenticated { username =>
    implicit request =>
      Ok(views.html.staticpages.administration(Messages("administration")))
  }

  /**
   * Display the history page.
   *
   * @return A response with HTTP status code 200 and the static history page.
   */
  def history = Action { implicit request =>
    Ok(views.html.staticpages.history(Messages("history")))
  }

  /**
   * Display the house page.
   *
   * @return A response with HTTP status code 200 and the static house page.
   */
  def house = Action { implicit request =>
    Ok(views.html.staticpages.house(Messages("house")))
  }

  /**
   * Display the page on the sports of the actives.
   *
   * @return A response with HTTP status code 200 and the static actives' sports page.
   */
  def sportsactives = Action { implicit request =>
    Ok(views.html.staticpages.sportsactives(Messages("sports.actives")))
  }

  /**
   * Display the rowing page.
   *
   * @return A response with HTTP status code 200 and the static rowing page.
   */
  def rowing = Action { implicit request =>
    Ok(views.html.staticpages.rowing(Messages("sports.rowing")))
  }

  /**
   * Display the handball page.
   *
   * @return A response with HTTP status code 200 and the static handball page.
   */
  def handball = Action { implicit request =>
    Ok(views.html.staticpages.handball(Messages("sports.handball")))
  }

  /**
   * Display the fistball page.
   *
   * @return A response with HTTP status code 200 and the static fistball page.
   */
  def fistball = Action { implicit request =>
    Ok(views.html.staticpages.fistball(Messages("sports.fistball")))
  }

  /**
   * Display the volleyball page.
   *
   * @return A response with HTTP status code 200 and the static volleyball page.
   */
  def volleyball = Action { implicit request =>
    Ok(views.html.staticpages.volleyball(Messages("sports.volleyball")))
  }
}