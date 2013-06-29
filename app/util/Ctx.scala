/**
 *
 */
package util

import controllers.UserCtrl
import play.api.mvc.Request
import models.User
import play.api.mvc.Security
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages
import display.Header
import display.Menu

/**
 * Context for requests.
 *
 * @author andreas
 * @version 0.0.3, 2013-06-29
 */
case class Ctx(header: Header, topMenu: Option[List[Menu]], sideMenu: Option[List[Menu]])(implicit request: Request[_]) {

  val user: Option[User] = getUser
  val referer = request.headers.get("referer")

  def getUser[A](implicit request: Request[A]): Option[User] = {
    val user = request.session.get(Security.username)
    if (user.isDefined) {
      val result = User.findByName(user.get)
      if (result.isSuccess) {
        result.toOption
      } else {
        None
      }
    } else {
      None
    }
  }
}

object Ctx {
  val loginForm = Form(
    tuple(
      "username" -> nonEmptyText,
      "password" -> nonEmptyText) verifying (Messages("error.loginFailed"), fields => fields match {
        case (e, p) => User.auth(e, p).toOption.isDefined
      }))
}