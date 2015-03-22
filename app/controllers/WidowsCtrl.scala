package controllers

import controllers.ext.ProvidesCtx
import controllers.ext.Security
import models.{ Person, Persons }
import play.api.i18n.Messages
import play.api.Logger
import play.api.mvc._
import play.api.mvc.Security._
import play.api.Play.current
import util.Witwe
import controllers.ext.SublistRetrieverAndAdder

object WidowsCtrl extends Controller with ProvidesCtx with Security with SublistRetrieverAndAdder {

  /**
   * Provide a list of all available person items.
   */
  def list = isAuthenticated { username =>
    implicit request =>
      var list: List[(String, List[Person])] = Nil

      list = this.getAndAddListOfMembersByStatus(list, "widows", Witwe)
      
      Ok(views.html.personList(list, Messages("widows")))
  }
}