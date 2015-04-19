package controllers

import controllers.ext.{ProvidesCtx,Security,SublistRetrieverAndAdder}
import models.Person
import play.api.i18n.Messages
import play.api.mvc._
import util.{MitturnerAk, MitturnerKV}

/**
 * Controller to handle requests on the organizational unit Mitturner.
 *
 * @author andreas
 * @version 0.0.2, 2015-04-19
 */
object MitturnerCtrl extends Controller with ProvidesCtx with Security with SublistRetrieverAndAdder {

  /**
   * Provide a list of all available person items.
   */
  def list = isAuthorized("view.person") { username =>
    implicit request =>
      var list: List[(String, List[Person])] = Nil
      list = this.getAndAddListOfMembersByStatus(list, "kv.aoms", MitturnerKV)
      list = this.getAndAddListOfMembersByStatus(list, "aktivitas.mitturner", MitturnerAk)
      
      Ok(views.html.personList(list, Messages("aktivitas.mitturner")))
  }
}