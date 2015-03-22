package controllers

import controllers.ext.ProvidesCtx
import controllers.ext.Security
import controllers.ext.SublistRetrieverAndAdder
import models.{ Person, Persons }
import play.api.i18n.Messages
import play.api.Logger
import play.api.mvc._
import play.api.mvc.Security._
import play.api.Play.current
import util.{ Aktiv, Inaktiv, Auswaertig, Fux, EM, MitturnerAk }

object AktivitasCtrl extends Controller with ProvidesCtx with Security with SublistRetrieverAndAdder {

   /**
   * Provide a list of all available person items.
   */
  def list = isAuthenticated { username =>
    implicit request =>
      var list: List[(String, List[Person])] = Nil

      list = this.getAndAddListOfMembersByStatus(list, "aktivitas.ems", EM)
      list = this.getAndAddListOfMembersByStatus(list, "aktivitas.mitturner", MitturnerAk)
      list = this.getAndAddListOfMembersByStatus(list, "aktivitas.fuxia", Fux)
      list = this.getAndAddListOfMembersByStatus(list, "aktivitas.auswaertig", Auswaertig);
      list = this.getAndAddListOfMembersByStatus(list, "aktivitas.inaktiv", Inaktiv);
      list = this.getAndAddListOfMembersByStatus(list, "aktivitas.aktiv", Aktiv);
      
      Ok(views.html.personList(list, Messages("aktivitas.members")))
  }
  
  
}