/**
 *
 */
package controllers

import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages
import play.api.Logger
import play.api.mvc._
import models.News
import controllers.ext.{ProvidesCtx,Security}


/**
 * Controller for all actions related to [[News]].
 * 
 * @author andreas
 * @version 0.0.6, 2015-04-17
 */
object NewsCtrl extends Controller with ProvidesCtx with Security {

  /**
   * Form to display, enter, and modify a [[News]] item.
   */
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

  /**
   * Create a new [[News]] item.
   *
   * @return A response with HTTP status code 200 carrying an empty form.
   */
  def create = isAuthorized("create.news") { username =>
    implicit request =>
      Ok(views.html.newsForm(newsForm))
  }

  /**
   * Delete the [[News]] item with the given <em>id</em>.
   * 
   * @param id Identifier of the [[News]] item to be deleted.
   * @return A redirect to display the list of [[News]] items flashing either success or error of the action undertaken.
   */
  def delete(id: Long) = isAuthorized("delete.news") { username =>
    implicit request =>
      val result = News.delete(id)
      if (result.isSuccess) {
        Logger.debug(s"Successfully deleted news with ID $id.")
        Redirect(routes.NewsCtrl.list()).flashing("success" -> Messages("success.deleting.news"))
      } else {
        Logger.error(result.toString, result.toEither.left.get)
        Redirect(routes.NewsCtrl.list()).flashing("error" -> Messages("error.deleting.news"))
      }
  }

  /**
   * Edit the [[News]] item with the given <em>id</em>.
   * 
   * @param id Identifier of the news item to be edited.
   * @return In case of successfully finding a [[News]] item matching the <em>id</em> a response with
   *         status code 200 and the details of the retrieved [[News]] item for a payload. In case of
   *         error a redirect to the list of [[News]] items is returned flashing an error message.
   */
  def edit(id: Long) = isAuthorized("edit.news") { username =>
    implicit request =>
      val n = News.load(id)
      n match {
        case None =>
          Logger.logger.debug(s"Cannot find news item with ID $id.")
          Redirect(routes.NewsCtrl.list()).flashing("error" -> Messages("error.finding.news"))
        case Some(news) =>
          Logger.logger.debug(s"Preparing editing of news item with ID $id.")
          Ok(views.html.newsForm(newsForm.fill(news)))
        case _ => NotFound
      }
  }

  /**
   * Retrieve 10 news items and display them.
   *
   * @return A redirect to display a list of the 10 latest [[News]] items.
   */
  def list = Action {
    implicit request =>
      Redirect(routes.NewsCtrl.listLtd(10))
  }

  /**
   * Retrieve the specified <em>limit</em> number of [[News]] items and display them.
   * 
   * @param limit The number of [[News]] items to retrieve.
   * @return A response with an HTTP status of 200 and a list of up to <em>limit</em> [[News]]
   *         items. In case of error, a response with an HTTP status code of 400 is returned.
   */
  def listLtd(limit: Int) = Action { implicit request =>

    val result = News.getAll(Some(limit))
    if (result.isSuccess) {
      Ok(views.html.newsList(result.toOption.get))
    } else {
      Logger.error(result.toString, result.toEither.left.get)
      BadRequest(Messages("error.loading.news.list"))
    }
  }

  /**
   * Select a single [[News]] item with the given <em>id</em> for display.
   * 
   * @param id The identifier of the [[News]] item to be fetched.
   * @return A response with HTTP status code 200 and a payload of the [[News]] item. In the case
   *         of error, a redirect to the list of [[News]] items is returned.
   */
  def show(id: Long) = Action { implicit request =>
    val e = News.load(id)
    e match {
      case None =>
        Logger.debug(s"No news item with ID $id found.")
        Redirect(routes.NewsCtrl.list()).flashing("error" -> Messages("error.loading.news"))
      case Some(ev) =>
        Logger.debug(s"Found news item with ID $id.")
        Ok(views.html.news(ev))
      case _ => NotFound
    }
  }

  /**
   * Store a new news item.
   *
   * @return In case of success a redirect to the details of the [[News]] item flashing a success
   *         message. Otherwise a response with status code 400 reloading the form with its input
   *         data and an appropriate error message.
   */
  def submit = isAuthorized("save.news") { username =>
    implicit request =>
      newsForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the news form.")
          BadRequest(views.html.newsForm(errors))
        },
        news => {
          Logger.debug(s"Storing news item $news.")
          val result = News.saveOrUpdate(news)
          if (result.isSuccess) {
            Redirect(routes.NewsCtrl.show(result.toOption.get.id.get)).flashing("success" -> Messages("success.storing.news"))
          } else {
            Logger.error(result.toString, result.toEither.left.get)
            BadRequest(views.html.newsForm(newsForm)).flashing("error" -> Messages("error.storing.news"))
          }
        })
  }
}