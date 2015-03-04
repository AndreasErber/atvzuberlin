/**
 *
 */
package controllers

import accesscontrol.{ Role, Roles, UserHasRole, UserHasRoles }
import util.CustomFormatters
import play.api.data.Form
import play.api.data.Forms._
import play.api.db._
import play.api.i18n.Messages
import play.api.Logger
import play.api.mvc._
import play.api.mvc.Security._
import play.api.Play.current
import controllers.ext.{ ProvidesCtx, Security }
import views.html.defaultpages.notFound
import accesscontrol.User
import accesscontrol.UserHasRole

/**
 * @author andreas
 * @version 0.0.3, 2015-01-03
 */
object UserHasRoleCtrl extends Controller with ProvidesCtx with Security {

  implicit val roleFormatter = CustomFormatters.roleFormatter

  val roleMapping = of[Role]

  val form = Form(
    tuple(
      "username" -> text,
      "roles" -> list(roleMapping)))

  def create(un: String) = isAuthenticated { username =>
    implicit request =>
      val roles = Roles.getAll
      if (roles.isSuccess) {
        val uhr = UserHasRoles.getByUser(un)
        if (uhr.isSuccess) {
          Ok(views.html.userHasRolesForm(form.fill((un, uhr.toOption.get.map(uhr => uhr.role))), roles.toOption.get.sortBy(r => r.name)))
        } else {
          Logger.logger.error("Failed to load existing user to role relations.")
          Redirect(routes.UserCtrl.show(un)).flashing("error" -> Messages("error.failedToLoadUserHasRoles", un))
        }
      } else {
        Logger.logger.error("Failed to load all roles.", roles.toEither.left.get)
        Redirect(routes.UserCtrl.show(un)).flashing("error" -> Messages("error.failedToLoadRoles"))
      }
  }

  def submit = isAuthenticated { username =>
    implicit request =>
      val l: List[User] = Nil
      Ok(views.html.usersList(l))

  }
  
  /**
   * TODO remove ... just for checking out things
   */
  def fun(userHasRoleForm: Form[(String, List[accesscontrol.Role])], uhr: List[accesscontrol.UserHasRole]) {
    
    val x = userHasRoleForm("roles")
    
    for (x <- userHasRoleForm.data) {
//      x._1 + " -> " x._2
    }
  }
}