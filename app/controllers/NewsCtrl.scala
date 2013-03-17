/**
 *
 */
package controllers

import play.api.data.Form
import play.api.data.Forms._
import play.api.Logger
import play.api.mvc._
import util.CustomFormatters
import util.UsageType
import models.{ News, NewsTable }
import play.api.Play.current
import controllers.ext.ProvidesCtx
import controllers.ext.Security
import play.api.i18n.Messages

/**
 * @author andreas
 * @version 0.0.1, 2013-03-16
 */
object NewsCtrl extends Controller with ProvidesCtx with Security {

  val newsForm = Form[News](
    mapping(
      "id" -> optional(longNumber),
      "title" -> nonEmptyText,
      "lead" -> optional(text),
      "content" -> optional(text),
      "created" -> longNumber,
      "creator" -> text,
      "modified" -> optional(longNumber),
      "modifier" -> optional(text))(News.apply)(News.unapply))

  def create = isAuthenticated { username =>
    implicit request =>
      Ok(views.html.newsForm(newsForm))
  }
  
  def delete(id: Long) = isAuthenticated { username =>
    implicit request =>
      val result = News.delete(id)
      if (result.isSuccess) {
        Logger.debug("Successfully deleted news with ID " + id + ".")
        Redirect(routes.NewsCtrl.list).flashing(("success" -> Messages("success.succeededToDeleteNews")))
      } else {
        Logger.error(result.toString(), result.fail.toOption.get)
        Redirect(routes.NewsCtrl.list).flashing(("error" -> Messages("error.failedToDeleteNews")))
      }
  }

  def edit(id: Long) = isAuthenticated { username =>
    implicit request =>
      val n = News.load(id)
      n match {
        case None =>
          Logger.logger.debug("Cannot find news item with ID " + id + "."); NotFound
        case Some(news) =>
          Logger.logger.debug("Preparing editing of news item with ID " + id + ".");
          Ok(views.html.newsForm(newsForm.fill(news)))
        case _ => NotFound
      }
  }
  
  def list = Action {
    implicit request =>
      Redirect(routes.NewsCtrl.listLtd(10))
  }
  
  def listLtd(limit: Int) = Action { implicit request =>
    
    val result = News.getAll(Some(limit))
    if (result.isSuccess) {
      val req = Ok(views.html.newsList(result.toOption.get))
      if (flash.get("error").isDefined) {
        req.flashing(("error" -> flash.get("error").get))
      } else if (flash.get("success").isDefined) {
        req.flashing(("success" -> flash.get("success").get))
      } else {
        req
      }
    } else {
      Logger.error(result.toString(), result.fail.toOption.get)
      Ok(views.html.newsList(List())).flashing("error" -> Messages("error.failedToLoadNewsList"))
    }
  }

  
  def show(id: Long) = Action { implicit request =>
    val e = News.load(id)
    e match {
      case None =>
        Logger.logger.debug("No news item with ID " + id + " found."); NotFound
      case Some(ev) =>
        Logger.logger.debug("Found news item with ID " + id + ".")
        val req = Ok(views.html.news(ev))
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
      newsForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the news form.")
          BadRequest(views.html.newsForm(errors))
        },
        event => {
          Logger.debug("Storing news item " + event)
          val result = News.saveOrUpdate(event)
          if (result.isSuccess) {
            Redirect(routes.NewsCtrl.show(result.toOption.get.id.get)).flashing("success" -> Messages("success.succeededToStoreNews"))
          } else {
            Logger.error(result.toString(), result.fail.toOption.get)
            BadRequest(views.html.newsForm(newsForm)).flashing("error"-> Messages("error.failedToStoreNews"))
          }
        })
  }
}