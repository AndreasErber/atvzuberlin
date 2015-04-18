/**
 *
 */
package controllers

import accesscontrol.{Privilege, Role, RoleHasPrivileges, Roles}
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages
import play.api.Logger
import play.api.mvc._
import controllers.ext.{ProvidesCtx, Security}

/**
 * Controller to handle requests dealing with [[Role]]s.
 *
 * @author andreas
 * @version 0.0.3, 2015-04-17
 */
object RoleCtrl extends Controller with ProvidesCtx with Security {

  /**
   * Form to display, enter, or modify a [[Role]].
   */
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
   *
   * @return A response with HTTP status code 200 and an empty [[Role]] form for a payload.
   */
  def create = isAuthenticated { username =>
    implicit request =>
      Ok(views.html.roleForm(roleForm))
  }

  /**
   * Delete the [[Role]] identified by the given <em>id</em>.
   *
   * @param id The identifier of the [[Role]] to delete.
   * @return A redirect to display the list of [[Role]]s flashing either success or error on the action executed.
   */
  def delete(id: Long) = isAuthenticated { username =>
    implicit request =>
      val result = Roles.delete(id)
      if (result > 0) {
        Logger.debug(s"Successfully deleted role with ID $id.")
        Redirect(routes.PrivilegeCtrl.list()).flashing("success" -> Messages("success.deleting.role"))
      } else {
        Logger.error(s"Failed to delete role with ID $id.")
        Redirect(routes.PrivilegeCtrl.list()).flashing("error" -> Messages("error.deleting.role"))
      }
  }

  /**
   * Modify an existing role item.
   *
   * @param id The identifier of the [[Role]] to retrieve for editing.
   * @return In case of successfully finding a [[Role]] matching the <em>id</em> a response with
   *         status code 200 and the details of the retrieved [[Role]] for a payload. In case of
   *         error a redirect to the list of [[Role]]s is returned flashing an error message.
   */
  def edit(id: Long) = isAuthenticated { username =>
    implicit request =>
      val p = Roles.get(id)
      p match {
        case None =>
          Logger.debug(s"Cannot find role with ID $id.")
          Redirect(routes.PrivilegeCtrl.list()).flashing("error" -> Messages("error.finding.role"))
        case Some(pers) =>
          Logger.debug(s"Preparing editing of role with ID $id.")
          Ok(views.html.roleForm(roleForm.fill(pers)))
        case _ => NotFound
      }
  }

  /**
   * Provide a list of all available role items.
   *
   * @return A response with HTTP status code 200 and the list of [[Role]]s for a payload. In case
   *         of error, a response with status code 400.
   */
  def list = isAuthenticated { username =>
    implicit request =>
      val list = Roles.getAll()
      if (list.isSuccess) {
        Ok(views.html.rolesList(list.toOption.get.sortBy(x => x.name)))
      } else {
        Logger.error(list.toString, list.toEither.left.get)
        BadRequest(Messages("error.loading.roles.list"))
      }
  }

  /**
   * Display the details of a [[Role]].
   *
   * @param id The identifier of the [[Role]] to display.
   * @return An HTTP response with status code 200 and the details of the [[Role]] found by the
   *         <em>id</em>. In case of error a redirect to the list of [[Role]]s is returned.
   */
  def show(id: Long) = isAuthenticated { username =>
    implicit request =>
      val roleVal = Roles.get(id)
      roleVal match {
        case None =>
          Logger.debug(s"No role with ID $id found.")
          Redirect(routes.RoleCtrl.list()).flashing("error" -> Messages("error.loading.role"))
        case Some(role) =>
          Logger.debug(s"Found role with ID $id: '${role.name}'. Getting its privileges ...")
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
            Logger.error(s"Failed to retrieve RoleHasPrivileges for role ${role.name}: " + rhpVal.toEither.left.get.getMessage)
          }
          Ok(views.html.role(role, privs.sortBy(priv => priv.name)))
        case _ => NotFound
      }
  }

  /**
   * Submit a new or modified role item.
   *
   * @return A redirect to display the details of the newly created [[Role]], in case of error a
   *         [[BadRequest]] displaying the error.
   */
  def submit = isAuthenticated { username =>
    implicit request =>
      roleForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the roles form.")
          BadRequest(views.html.roleForm(errors))
        },
        p => {
          val result = Roles.saveOrUpdate(p)
          if (result.isSuccess) {
            Redirect(routes.RoleCtrl.show(result.toOption.get.id.get)).flashing("success" -> Messages("success.storing.role"))
          } else {
            Logger.error(result.toString, result.toEither.left.get)
            BadRequest(views.html.roleForm(roleForm)).flashing("error" -> Messages("error.storing.role"))
          }
        })
  }
}