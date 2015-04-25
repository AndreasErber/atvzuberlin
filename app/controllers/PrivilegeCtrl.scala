/**
 *
 */
package controllers

import play.api.data.Form
import play.api.data.Forms._
import play.api.Logger
import play.api.mvc._
import accesscontrol.{Privilege, Privileges, RoleHasPrivilege, RoleHasPrivileges, Roles}
import controllers.ext.ProvidesCtx
import controllers.ext.Security
import play.api.i18n.Messages

import scalaz.{Failure, Success}

/**
 * Controller to handle requests for [[Privilege]]s.
 *
 * @author andreas
 * @version 0.0.6, 2015-04-25
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
  def create = isAuthorized("create.privilege") { username =>
    implicit request =>
      Ok(views.html.privilegeForm(privilegeForm))
  }

  /**
   * Delete the [[Privilege]] identified by <em>id</em>
   *
   * @param id The identifier of the [[Privilege]] to delete.
   * @return
   */
  def delete(id: Long) = isAuthorized("delete.privilege") { username =>
    implicit request =>
      val privV = Privileges.get(id)
      privV match {
        case Success(privOp) => privOp match {
          case Some(priv) => RoleHasPrivileges.deleteByPrivilege(priv)
            val result = Privileges.delete(id)
            if (result > 0) {
              val msg = s"Sucessfully deleted privilege with ID $id."
              Logger.debug(msg)
              Redirect(routes.PrivilegeCtrl.list()).flashing("success" -> msg)
            } else {
              val msg = Messages("error.failedToDeletePrivilege")
              Logger.error(msg)
              Redirect(routes.PrivilegeCtrl.list()).flashing("error" -> msg)
            }
          case None => Logger.error(s"Failed to load privilege with ID '$id'. Does not exist.")
            Redirect(routes.PrivilegeCtrl.list()).flashing("error" -> Messages("error.privilege.does.not.exist", id))
        }
        case Failure(t) => Logger.error(privV.toString, t)
          Redirect(routes.PrivilegeCtrl.list()).flashing("error" -> Messages("error.loading.privilege"))
      }
  }

  /**
   * Provide a list of all available person items.
   */
  def list = isAuthorized("view.privilege") { username =>
    implicit request =>
      val list = Privileges.getAll
      if (list.isSuccess) {
        Ok(views.html.privilegesList(list.toOption.get.sortBy(x => x.name)))
      } else {
        Logger.logger.error(list.toString, list.toEither.left.get)
        BadRequest("When trying to load the list of privileges a failure occurred.")
      }
  }

  /**
   * Display the form with the [[Privilege]] data identified by the given <em>id</em>.
   *
   * @param id Identifier of the [[Privilege]] to load into the form.
   * @return
   */
  def edit(id: Long) = isAuthorized("edit.privilege") { username =>
    implicit request =>
      val privV = Privileges.get(id)
      privV match {
        case Success(privOp) => privOp match {
          case Some(priv) => Ok(views.html.privilegeForm(privilegeForm.fill(priv)))
          case None => Logger.error(s"Failed to load privilege with ID '$id'. Does not exist.")
            Redirect(routes.PrivilegeCtrl.list()).flashing("error" -> Messages("error.loading.privilege"))
        }
        case Failure(t) => Logger.error(privV.toString, t)
          Redirect(routes.PrivilegeCtrl.list()).flashing("error" -> Messages("error.loading.privilege"))
      }
  }

  /**
   * Receive and handle a new or modified privilege.
   */
  def submit() = isAuthorized("save.privilege") { username =>
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
                  Logger.error("Failed to insert RoleHasPrivilege relation due to: " + rhpSaved.toEither.left.get.getMessage)
                }
              } else {
                Logger.logger.error("Administrator role was not found. Privilege could not be added.")
              }
            }
          } else {
            Logger.logger.error(result.toString, result.toEither.left.get)
          }
          Redirect(routes.PrivilegeCtrl.list())
      } getOrElse BadRequest
  }

  /**
   * Update a [[Privilege]].
   * @return
   */
  def updatePrivilege() = isAuthorized("save.privilege") { username =>
    implicit request =>
      privilegeForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the privilege form.")
          BadRequest(views.html.privilegeForm(errors))
        },
        privilege => {
          Logger.debug("Storing privilege " + privilege.name)
          val resultV = Privileges.saveOrUpdate(privilege)
          resultV match {
            case Success(result) =>  Redirect(routes.PrivilegeCtrl.list())
            case Failure(t) => Logger.error(resultV.toString, t)
            BadRequest(views.html.privilegeForm(privilegeForm.fill(privilege)))
          }
        })
  }
}