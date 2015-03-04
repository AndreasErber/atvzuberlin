/**
 *
 */
package controllers

import play.api.data.Form
import play.api.data.Forms._
import play.api.Logger
import play.api.mvc._
import util.CustomFormatters
import util.UsageType
import accesscontrol.{ Privilege, Privileges }
import play.api.Play.current
import controllers.ext.ProvidesCtx
import controllers.ext.Security
import play.api.i18n.Messages
import models.Organization
import accesscontrol.Privileges
import accesscontrol.Roles
import accesscontrol.RoleHasPrivileges
import accesscontrol.RoleHasPrivilege
import accesscontrol.RoleHasPrivilege

/**
 * @author andreas
 * @version 0.0.3, 2015-01-04
 */
object PrivilegeCtrl extends Controller with ProvidesCtx with Security {

  val privilegeForm = Form(
    mapping(
      "id" -> optional(longNumber),
      "name" -> text,
      "description" -> optional(text),
      "created" -> longNumber,
      "creator" -> text,
      "modified" -> optional(longNumber),
      "modifier" -> optional(text))(Privilege.apply)(Privilege.unapply))

  /**
   * Display the form to create a new privilege.
   */
  def create = isAuthenticated { username =>
    implicit request =>
      Ok(views.html.privilegeForm(privilegeForm))
  }

  def delete(id: Long) = isAuthenticated { username =>
    implicit request =>
      val privVal = Privileges.get(id)
      if (privVal.isDefined) {
        RoleHasPrivileges.deleteByPrivilege(privVal.get)
        val result = Privileges.delete(id)
        if (result > 0) {
          val msg = s"Sucessfully deleted privilege with ID $id."
          Logger.debug(msg)
          Redirect(routes.PrivilegeCtrl.list).flashing("success" -> msg)
        } else {
          val msg = Messages("error.failedToDeletePrivilege")
          Logger.error(msg)
          Redirect(routes.PrivilegeCtrl.list).flashing("error" -> msg)
        }
      } else {
        val msg = Messages("error.privilege.does.not.exist", id)
        Logger.error(msg)
        Redirect(routes.PrivilegeCtrl.list).flashing("error" -> msg)
      }
  }

  /**
   * Provide a list of all available person items.
   */
  def list = isAuthenticated { username =>
    implicit request =>
      val list = Privileges.getAll
      if (list.isSuccess) {
        Ok(views.html.privilegesList(list.toOption.get.sortBy(x => x.name)))
      } else {
        Logger.logger.error(list.toString(), list.toEither.left.get)
        BadRequest("When trying to load the list of privileges a failure occurred.")
      }
  }

  def edit(id: Long) = isAuthenticated { username =>
    implicit request =>
      val p = Privileges.get(id)
      p match {
        case None =>
          Logger.logger.debug("Cannot find privilege with ID " + id + "."); NotFound
        case Some(pers) =>
          Logger.logger.debug("Preparing editing of privilege with ID " + id + ".");
          Ok(views.html.privilegeForm(privilegeForm.fill(pers)))
        case _ => NotFound
      }
  }

  /**
   * Receive and handle a new or modified privilege.
   */
  def submit = isAuthenticated { username =>
    implicit request =>
      privilegeForm.bindFromRequest.value map {
        p =>
          // store the new privilege
          val result = Privileges.saveOrUpdate(p)
          if (result.isSuccess) {
            // when successful then add the privilege to the administrator
            val adminRoleVal = Roles.getByName("Administrator")
            if (adminRoleVal.isFailure) {
              Logger.logger.error("Failed to determine Administrator role due to: " + adminRoleVal.toEither.left.get)
            } else {
              val adminRole = adminRoleVal.getOrElse(None)
              if (adminRole.isDefined) {
                val rhp = RoleHasPrivilege(None, adminRole.get, result.toOption.get, System.currentTimeMillis(), username, None, None)
                val rhpSaved = RoleHasPrivileges.saveOrUpdate(rhp)
                if (rhpSaved.isFailure) {
                  Logger.error("Failed to insert RoleHasPrivilege relation due to: " + rhpSaved.toEither.left.get.getMessage())
                }
              } else {
                Logger.logger.error("Administrator role was not found. Privilege could not be added.")
              }
            }
          } else {
            Logger.logger.error(result.toString(), result.toEither.left.get)
          }
          Redirect(routes.PrivilegeCtrl.list)
      } getOrElse BadRequest
  }

  def updatePrivilege = isAuthenticated { username =>
    implicit request =>
      privilegeForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the privilege form.")
          BadRequest(views.html.privilegeForm(errors))
        },
        privilege => {
          Logger.debug("Storing privilege " + privilege.name)
          val result = Privileges.saveOrUpdate(privilege)
          if (result.isSuccess) {
            Redirect(routes.PrivilegeCtrl.list)
          } else {
            Logger.error(result.toString(), result.toEither.left.get)
            BadRequest(views.html.privilegeForm(privilegeForm.fill(privilege)))
          }
        })
  }
}