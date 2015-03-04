/**
 *
 */
package util

import controllers.UserCtrl
import play.api.mvc.Request
import accesscontrol.User
import play.api.mvc.Security
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages
import display.Header
import display.Menu
import accesscontrol.Privileges
import accesscontrol.UserHasRoles
import accesscontrol.RoleHasPrivileges
import play.Logger
import accesscontrol.Privilege

/**
 * Context for requests.
 *
 * @param header The page header.
 * @param topMenu The head menu of the page.
 * @param sideMenu The side navigation of the pase.
 * @param request The current request that is handled.
 * @author andreas
 * @version 0.0.4, 2015-01-05
 */
case class Ctx(header: Header, topMenu: Option[List[Menu]], sideMenu: Option[List[Menu]])(implicit request: Request[_]) {

  val user: Option[User] = getUser
  val privilegesOfUser: Option[List[Privilege]] = getUserPrivileges(user)
  val referer = request.headers.get("referer")

  /**
   * Retrieve the {@link User} instance identified by the username in the HTTP session.
   *
   * @returns An optional {@link User} instance if a match is found.
   */
  def getUser[A](implicit request: Request[A]): Option[User] = {
    val user = request.session.get(Security.username)
    if (user.isDefined) {
      val result = User.findByName(user.get)
      if (result.isSuccess) {
        result.toOption
      } else {
        None
      }
    } else {
      None
    }
  }

  /**
   * Determines whether the given <em>user</em> has the specified <em>privilege</em>.
   *
   * The method will return <code>true</code> only in case it can be determined that the given <em>user</em> has the
   * specified <em>privilege</em>. If execution of intermediate steps fails, intermediate results are empty or the
   * <em>user</em> does not have the privilege <code>false</code> will be returned.
   *
   * @param user The {@link User} instance to check the privilege for.
   * @param privilege The name of the privilege to check for.
   * @returns <code>true</code> if the given <em>user</em> has the specified <em>privilege</em>, <code>false</code>
   *   otherwise.
   */
  def userHasPrivilege(user: User, privilege: String): Boolean = {
    if (!this.privilegesOfUser.isDefined) {
      return false
    }

    if (this.privilegesOfUser.get.map(priv => priv.name).contains(privilege)) {
      Logger.debug(s"User '${user.username}' has the privilege '$privilege'.")
      true
    } else {
      Logger.debug(s"User '${user.username}' does not have the privilege '$privilege'.")
      false
    }
  }

  /**
   * Retrieve the {@link Privilege}s the specified <em>user</em> has.
   *
   * The method queries for the {@link Role}s the <em>user</em> has and then queries for the {@link Privilege}s
   * associated with each {@link Role}.
   *
   * @param user The {@link User} to get the {@link Privilege}s for.
   * @returns Either {@link Some} possibly empty {@link List} of {@link Privilege}s or {@link None} in case of an
   *          error.
   */
  def getUserPrivileges(user: Option[User]): Option[List[Privilege]] = {
    if (!user.isDefined) {
      return Some(Nil)
    }
    val uhrVal = UserHasRoles.getByUser(user.get)
    if (uhrVal.isFailure) {
      Logger.error(s"Failed to retrieve UserHasRoles for user '${user.get.username}': " + uhrVal.toEither.left.get)
      return None
    }

    val uhrList = uhrVal.getOrElse(Nil)
    if (uhrList.isEmpty) {
      Logger.error(s"The list of UserHasRoles for user '${user.get.username}' is empty.")
      return Some(Nil)
    }

    val rhpVal = RoleHasPrivileges.getSome(uhrList.map(uhr => uhr.role))
    if (rhpVal.isFailure) {
      Logger.error("Failed to retrieve RoleHasPrivileges for some roles: " + rhpVal.toEither.left.get)
      return None
    }

    val rhpList = rhpVal.getOrElse(Nil)
    if (rhpList.isEmpty) {
      Logger.error(s"The list of RoleHasPrivileges is empty.")
      return Some(Nil)
    }

    Some(rhpList.map(rhp => rhp.privilege))
  }
}
