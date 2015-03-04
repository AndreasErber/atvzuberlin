/**
 *
 */
package controllers

import accesscontrol.{ Role, Roles }
import util.CustomFormatters
import play.api.data.Form
import play.api.data.Forms._
import play.api.db._
import play.api.Logger
import play.api.mvc._
import play.api.mvc.Security._
import play.api.Play.current
import controllers.ext.ProvidesCtx
import controllers.ext.Security
import accesscontrol.RoleHasPrivileges
import accesscontrol.Privilege

/**
 * Controller to handle requests dealing with {@link Role}s.
 * 
 * @author andreas
 * @version 0.0.2, 2015-01-04
 */
object RoleCtrl extends Controller with ProvidesCtx with Security {

  val roleForm = Form(
    mapping(
      "id" -> optional(longNumber),
      "name" -> text,
      "description" -> optional(text),
      "created" -> longNumber,
      "creator" -> text,
      "modified" -> optional(longNumber),
      "modifier" -> optional(text))(Role.apply)(Role.unapply))

  /**
   * Display the form to create a new privilege.
   */
  def create = isAuthenticated { username =>
    implicit request =>
      Ok(views.html.roleForm(roleForm))
  }

  def delete(id: Long) = isAuthenticated { username =>
    implicit request =>
      val result = Roles.delete(id)
      if (result > 0) {
        Logger.logger.debug("Sucessfully deleted role with ID " + id + ".")
      } else {
        Logger.logger.error("Failed to delete role with ID {}", id)
      }
      Redirect(routes.PrivilegeCtrl.list)
  }

  /**
   * Modify an existing role item.
   */
  def edit(id: Long) = isAuthenticated { username =>
    implicit request =>
      val p = Roles.get(id)
      p match {
        case None =>
          Logger.logger.debug("Cannot find role with ID " + id + "."); NotFound
        case Some(pers) =>
          Logger.logger.debug("Preparing editing of role with ID " + id + ".");
          Ok(views.html.roleForm(roleForm.fill(pers)))
        case _ => NotFound
      }
  }

  /**
   * Provide a list of all available role items.
   */
  def list = isAuthenticated { username =>
    implicit request =>
      val list = Roles.getAll
      if (list.isSuccess) {
        Ok(views.html.rolesList(list.toOption.get.sortBy(x => x.name)))
      } else {
        Logger.logger.error(list.toString(), list.toEither.left.get)
        BadRequest("When trying to load the list of roles a failure occurred.")
      }
  }

  /**
   * Display the details of a role.
   */
  def show(id: Long) = isAuthenticated { username =>
    implicit request =>
      val roleVal = Roles.get(id)
      roleVal match {
        case None =>
          Logger.logger.debug("No role with ID " + id + " found."); 
          NotFound
        case Some(role) =>
          Logger.logger.debug(s"Found role with ID $id: '${role.name}'. Getting its privileges ...")
          val rhpVal = RoleHasPrivileges.getSome(List(role))
          var privs: List[Privilege] = Nil
          if (rhpVal.isSuccess) {
            val rhpOp = rhpVal.toOption
            if (rhpOp.isDefined) {
              privs = rhpOp.get.map(rhp => rhp.privilege)
            } else {
              Logger.error(s"No privileges found for role ${role.name}")
            }
          } else {
            Logger.error(s"Failed to retrieve RoleHasPrivileges for role ${role.name}: " + rhpVal.toEither.left.get.getMessage())
          }
          Ok(views.html.role(role, privs.sortBy(priv => priv.name)))
        case _ => NotFound
      }
  }

  /**
   * Submit a new or modified role item.
   */
  def submit = isAuthenticated { username =>
    implicit request =>
      roleForm.bindFromRequest.value map {
        p =>
          val result = Roles.saveOrUpdate(p)
          if (result.isSuccess) {
            Redirect(routes.RoleCtrl.list)
          } else {
            Logger.logger.error(result.toString(), result.toEither.left.get)
            Redirect(routes.RoleCtrl.list)
          }

      } getOrElse BadRequest
  }
}