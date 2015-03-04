/**
 *
 */
package controllers.ext

import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Request
import play.api.mvc.Result
import play.api.mvc.Security._
import accesscontrol.Privilege
import accesscontrol.Privileges
import accesscontrol.UserHasRole
import accesscontrol.UserHasRoles
import accesscontrol.RoleHasPrivileges
import accesscontrol.Privilege
import play.api.libs.iteratee.Done
import play.api.mvc.Results
import play.api.mvc.EssentialAction
import views.html.defaultpages.unauthorized
import play.Logger

/**
 * @author andreas
 * @version 0.0.3, 2014-11-22
 */
trait Security {

  def isAuthenticated(f: => String => Request[AnyContent] => Result) = {
    Authenticated { user =>
      Action(request => f(user)(request))
    }
  }

  def isAuthorized(requiredPrivilege: String)(f: => String => Request[AnyContent] => Result) = {
    Authenticated { user =>
      if (checkAuthorization(user, requiredPrivilege)) {
        Logger.info(s"User $user is authorized for the action.")
        Action(request => f(user)(request))
      } else {
        Logger.info("Action not authorized.")
        EssentialAction(request => Done(Results.Unauthorized))
      }
    }
  }

  /**
   * Check if the given <em>user</em> has the specified <em>requiredPrivilege</em>.
   *
   * @param user The username to check. Must neither be <code>null</code> nor empty.
   * @param requiredPrivilege The privilege to match.
   * @return <code>true</code> if <em>user</em> has a role that has a privilege named <em>requiredPrivilege</em>.
   */
  private def checkAuthorization(user: String, requiredPrivilege: String): Boolean = {
    Logger.debug(s"Checking authorization of '$user' for privilege '$requiredPrivilege")
    if (!Option(user).isDefined || user.isEmpty()) {
      Logger.error("No username was defined. Cannot check authorization.")
      return false
    }

    if (!Option(requiredPrivilege).isDefined || requiredPrivilege.isEmpty()) {
      Logger.error("No required privilege was defined. Cannot check authorization.")
      return false
    }

    // get the roles of the given user
    val uhrResult = UserHasRoles.getByUser(user)
    if (uhrResult isSuccess) {
      val uhrs = uhrResult.toOption.get
      if (uhrs.size <= 0) {
        Logger.debug(s"Found no roles for user $user. Not authorized.")
        return false
      }

      // retrieve the relational objects involving the given roles
      val rhpResult = RoleHasPrivileges.getSome(uhrs.map(_.role))

      if (rhpResult isSuccess) {
        val rhps = rhpResult.toOption.get
        if (rhps.size <= 0) {
          Logger.debug(s"Found no privileges for user $user. Not authorized.")
          return false
        }

        Logger.debug("User has " + rhps.size + " privileges: " +
          rhps.map(rhp => rhp.privilege.id.get + ": " + rhp.privilege.name + " - " + rhp.privilege.description)
          .mkString("\n", "\n", ""))

        // check if the user's role is appropriately privileged
        return this.containsPrivilege(rhps.map(rhp => rhp.privilege.name), requiredPrivilege)
      } else {
        Logger.debug("Query for privileges failed: " + rhpResult.fold(x => x.getMessage(), x => ""))
      }
    } else {
      Logger.error("Query for roles failed: " + uhrResult.fold(x => x.getMessage(), x => ""))
    }
    false
  }

  private def containsPrivilege(privileges: List[String], privilege: String): Boolean = {
    // check if the user's role is appropriately privileged
    for (priv <- privileges) {
      if (priv == privilege) {
        Logger.debug(s"Found match for privilege '$privilege': $priv")
        return true
      }
    }
    Logger.debug(s"Found no match for privilege '$privilege'.")
    false
  }

}