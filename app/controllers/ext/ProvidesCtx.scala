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
import models.Sports

/**
 * @author andreas
 * @version 0.0.3, 2013-06-29
 */
trait ProvidesCtx {

  implicit def getCtxt[A](implicit request: Request[A]): Ctx = {
    val sports = Sports.getAll
    var sportsMenu: List[MenuItem] = Nil
    if (sports.isSuccess) {
      sportsMenu = sports.toOption.get.map(s => MenuItem(s.title, "/sports/" + s.id.get))
    }
    val publicMenus = List(
      Menu(Messages("about"), "/about",
        List(
          MenuItem(Messages("about"), "/about"),
          MenuItem(Messages("house"), "/house"),
          MenuItem(Messages("history"), "/history"))),
      Menu(Messages("program"), "/events", List()),
      Menu(Messages("sports"), "/sports",
        List(
          MenuItem(Messages("sports"), "/sports/list"),
          MenuItem(Messages("sports.dates"), "/sportsdate/list")) ++ sportsMenu),
      Menu(Messages("members"), "/members", List()),
      Menu(Messages("organizations"), "/organizations", List()))

    val privateMenus = List(
      Menu(Messages("persons"), "/persons", List()),
      Menu(Messages("documents"), "/documents", List()),
      Menu(Messages("organizations"), "/organizations", List()),
      Menu(Messages("administration"), "/administration",
        List(
          MenuItem(Messages("administration.overview"), "/administration"),
          MenuItem(Messages("charges"), "/charge/list"),
          MenuItem(Messages("sports"), ""))))

    Ctx(Header(), Some(publicMenus), Some(privateMenus))
  }
}