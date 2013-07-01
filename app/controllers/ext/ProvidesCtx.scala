/**
 *
 */
package controllers.ext

import play.api.mvc.Request
import util.Ctx
import display.Header
import display.Menu
import play.api.i18n.Messages
import display.MenuItem

/**
 * @author andreas
 * @version 0.0.3, 2013-06-29
 */
trait ProvidesCtx {

  implicit def getCtxt[A](implicit request: Request[A]): Ctx = {
    val publicMenus = List(
      Menu(Messages("about"), "/about",
        List(
          MenuItem(Messages("about"), "/about"),
          MenuItem(Messages("house"), "/house"),
          MenuItem(Messages("history"), "/history"))),
      Menu(Messages("program"), "/events", List()),
      Menu(Messages("sports"), "/sports",
        List(
          MenuItem(Messages("sports"), "/sports"),
          MenuItem(Messages("sports.dates"), "/sportsdates"),
          MenuItem(Messages("sports.actives"), "/sportsactives"),
          MenuItem(Messages("sports.rowing"), "/rowing"),
          MenuItem(Messages("sports.handball"), "/handball"),
          MenuItem(Messages("sports.volleyball"), "/volleyball"))),
      Menu(Messages("members"), "/members", List()),
      Menu(Messages("organizations"), "/organizations", List()))

    val privateMenus = List(
      Menu(Messages("persons"), "/persons", List()),
      Menu(Messages("documents"), "", List()),
      Menu(Messages("organizations"), "/organizations", List()))

    Ctx(Header(), Some(publicMenus), Some(privateMenus))
  }
}