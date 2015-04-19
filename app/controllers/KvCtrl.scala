package controllers

import controllers.ext.{ProvidesCtx,Security,SublistRetrieverAndAdder}
import models.Person
import play.api.i18n.Messages
import play.api.mvc._
import util.{ Ehrenhalber, KV, MitturnerKV }

/**
 * Controller to handle requests on the organizational unit KV.
 *
 * @author andreas
 * @version 0.0.2, 2015-04-19
 */
object KvCtrl extends Controller with ProvidesCtx with Security with SublistRetrieverAndAdder {

  /**
   * Provide a list of all available person items.
   */
  def list = isAuthorized("view.person") { username =>
    implicit request =>
      var list: List[(String, List[Person])] = Nil
      list = this.getAndAddListOfMembersByStatus(list, "kv.aoms", MitturnerKV)
      list = this.getAndAddListOfMembersByStatus(list, "kv", KV)
      list = this.getAndAddListOfMembersByStatus(list, "kv.ehrenhalber", Ehrenhalber)
      
      Ok(views.html.personList(list, Messages("kv.members")))
  }
}