/**
 *
 */
package controllers.ext

import controllers.routes
import play.api.mvc.Request
import util.{ Article, Agenda, Charter, Circular, Ctx, Database, Form, Miscellaneous, Protocol }
import display.Header
import display.Menu
import play.api.i18n.Messages
import models.Sports
import display.MenuItem

/**
 * @author andreas
 * @version 0.0.6, 2015-04-06
 */
trait ProvidesCtx {

  implicit def getCtxt[A](implicit request: Request[A]): Ctx = {
    val sports = Sports.getAll
    var sportsMenu: List[MenuItem] = Nil
    if (sports.isSuccess) {
      sportsMenu = sports.toOption.get.map(s => MenuItem(s.title, routes.SportCtrl.showSports(s.id.get).toString()))
    }
    val publicMenus = List(
      Menu(Messages("about"), routes.Application.about().toString(),
        List(
          MenuItem(Messages("about"), routes.Application.about().toString()),
          MenuItem(Messages("house"), routes.Application.house().toString()),
          MenuItem(Messages("history"), routes.Application.history().toString()))),
      Menu(Messages("program"), routes.EventCtrl.listUpcoming().toString(), List()),
      Menu(Messages("sports"), routes.SportCtrl.listSports().toString(),
        List(
          MenuItem(Messages("sports"), routes.SportCtrl.listSports().toString()),
          MenuItem(Messages("sports.dates"), routes.SportCtrl.listSportsDates().toString())) ++ sportsMenu),
      Menu(Messages("organizations"), routes.OrganizationCtrl.list().toString(), List()))

    val privateMenus = List(
      Menu(Messages("news"), routes.NewsCtrl.list().toString(), List(
          MenuItem(Messages("news.view"), routes.NewsCtrl.list().toString()),
          MenuItem(Messages("news.add"), routes.NewsCtrl.create().toString())
          )),
      Menu(Messages("personalia"), routes.PersonCtrl.list().toString(), List(
          MenuItem(Messages("aktivitas"), routes.AktivitasCtrl.list().toString()),
          MenuItem(Messages("kv"), routes.KvCtrl.list().toString()),
          MenuItem(Messages("mitturner"), routes.MitturnerCtrl.list().toString()),
          MenuItem(Messages("widows"), routes.WidowsCtrl.list().toString()),
          MenuItem(Messages("person.others"), routes.OtherPersonsCtrl.list().toString()),
          MenuItem(Messages("person.add"), routes.PersonCtrl.create().toString())
          )),
      Menu(Messages("documents"), routes.DocumentCtrl.overview().toString(), List(
        MenuItem(Messages("documents.overview.all"), routes.DocumentCtrl.overview().toString()),
        MenuItem(Messages("documents.protocol.pl"), routes.DocumentCtrl.listByCategory(Protocol.name).toString()),
        MenuItem(Messages("documents.article.pl"), routes.DocumentCtrl.listByCategory(Article.name).toString()),
        MenuItem(Messages("documents.agenda.pl"), routes.DocumentCtrl.listByCategory(Agenda.name).toString()),
        MenuItem(Messages("documents.circular.pl"), routes.DocumentCtrl.listByCategory(Circular.name).toString()),
        MenuItem(Messages("documents.charter.pl"), routes.DocumentCtrl.listByCategory(Charter.name).toString()),
        MenuItem(Messages("documents.form.pl"), routes.DocumentCtrl.listByCategory(Form.name).toString()),
        MenuItem(Messages("documents.database.pl"), routes.DocumentCtrl.listByCategory(Database.name).toString()),
        MenuItem(Messages("documents.miscellaneous.pl"), routes.DocumentCtrl.listByCategory(Miscellaneous.name).toString()),
        MenuItem(Messages("document.upload"), routes.DocumentCtrl.create().toString())
      )),
      Menu(Messages("organizations"), routes.OrganizationCtrl.list().toString(), List()),
      Menu(Messages("administration"), routes.Application.administration().toString(),
        List(
          MenuItem(Messages("administration.overview"), routes.Application.administration().toString()),
          MenuItem(Messages("charges"), routes.ChargeCtrl.list().toString()),
          MenuItem(Messages("users"), routes.UserCtrl.list().toString()),
          MenuItem(Messages("roles"), routes.RoleCtrl.list().toString()),
          MenuItem(Messages("privileges"), routes.PrivilegeCtrl.list().toString()),
          MenuItem(Messages("sports"), routes.SportCtrl.listSports().toString()),
          MenuItem(Messages("academicTitles"), routes.AcademicTitleCtrl.list().toString()))))

    Ctx(Header(), Some(publicMenus), Some(privateMenus))
  }
}