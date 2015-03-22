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
import display.MenuItem

/**
 * @author andreas
 * @version 0.0.5, 2015-01-03
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
      Menu(Messages("organizations"), "/organizations", List()))

    val privateMenus = List(
      Menu(Messages("news"), "/newslist", List(
          MenuItem(Messages("news.view"), "/newslist"),
          MenuItem(Messages("news.add"), "/news/new")
          )),
      Menu(Messages("personalia"), "/persons", List(
          MenuItem(Messages("aktivitas"), "/aktivitas"),
          MenuItem(Messages("kv"), "/kv"),
          MenuItem(Messages("mitturner"), "/mitturner"),
          MenuItem(Messages("widows"), "/widows"),
          MenuItem(Messages("person.others"), "/others"),
          MenuItem(Messages("person.add"), "/person/new")
          )),
      Menu(Messages("documents"), "/documents", List()),
      Menu(Messages("organizations"), "/organizations", List()),
      Menu(Messages("administration"), "/administration",
        List(
          MenuItem(Messages("administration.overview"), "/administration"),
          MenuItem(Messages("charges"), "/charge/list"),
          MenuItem(Messages("users"), "/users"),
          MenuItem(Messages("roles"), "/roles"),
          MenuItem(Messages("privileges"), "/privileges"),
          MenuItem(Messages("sports"), "/sports/list"),
          MenuItem(Messages("academicTitles"), "/ats"))))

    Ctx(Header(), Some(publicMenus), Some(privateMenus))
  }
}