package controllers

import controllers.ext.{ ProvidesCtx, Security, SublistRetrieverAndAdder}
import models.Person
import play.api.i18n.Messages
import play.api.mvc._
import util.{ ATB, Other }

/**
 * Controller to handle requests to display [[Person]]s not assigned to another organizational unit.
 *
 * @version 0.0.2, 2015-04-20
 */
object OtherPersonsCtrl extends Controller with ProvidesCtx with Security with SublistRetrieverAndAdder {

  /**
   * Provide a list of all available person items.
   */
  def list = isAuthenticated { username =>
    implicit request =>
      var list: List[(String, List[Person])] = Nil
      list = this.getAndAddListOfMembersByStatus(list, "person.others", Other)
      list = this.getAndAddListOfMembersByStatus(list, "atb", ATB)
      
      Ok(views.html.personList(list, Messages("person.others")))
  }
}