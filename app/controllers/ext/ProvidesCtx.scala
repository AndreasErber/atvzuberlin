/**
 *
 */
package controllers.ext

import play.api.mvc.Request
import util.Ctx
import display.Header
import display.Menu
import play.api.i18n.Messages

/**
 * @author andreas
 *
 */
trait ProvidesCtx {

  implicit def getCtxt[A](implicit request: Request[A]): Ctx = {
    val menus = List(
        Menu(Messages("persons"), "/persons", List()), 
        Menu(Messages("program"), "/events", List()), 
        Menu(Messages("organizations"), "/organizations", List()),
        Menu(Messages("about"), "", List()),
        Menu(Messages("imprint"), "", List())
    )
    Ctx(Header(), Some(menus))
  }
}