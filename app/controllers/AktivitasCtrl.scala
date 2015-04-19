package controllers

import controllers.ext.{ProvidesCtx, Security, SublistRetrieverAndAdder}
import models.Person
import play.api.i18n.Messages
import play.api.Logger
import play.api.mvc._
import util.{Aktiv, Inaktiv, Auswaertig, Fux, EM, MitturnerAk}

/**
 * Controller to handle requests for an organizational unit.
 *
 * @author andreas
 * @version 0.0.3, 2015-04-19
 */
object AktivitasCtrl extends Controller with ProvidesCtx with Security with SublistRetrieverAndAdder {

  /**
   * Provide a list of all available [[Person]]s for this organizational unit.
   *
   * @return A response with HTTP status code 200 and a list of a list of [[Person]]s
   */
  def list = isAuthorized("view.person") { username =>
    implicit request =>
      Logger.debug(s"Displaying the list of actives.")

      var list: List[(String, List[Person])] = Nil
      list = this.getAndAddListOfMembersByStatus(list, "aktivitas.ems", EM)
      list = this.getAndAddListOfMembersByStatus(list, "aktivitas.mitturner", MitturnerAk)
      list = this.getAndAddListOfMembersByStatus(list, "aktivitas.fuxia", Fux)
      list = this.getAndAddListOfMembersByStatus(list, "aktivitas.auswaertig", Auswaertig)
      list = this.getAndAddListOfMembersByStatus(list, "aktivitas.inaktiv", Inaktiv)
      list = this.getAndAddListOfMembersByStatus(list, "aktivitas.aktiv", Aktiv)

      Ok(views.html.personList(list, Messages("aktivitas.members")))
  }
}