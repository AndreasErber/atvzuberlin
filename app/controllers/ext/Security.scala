/**
 *
 */
package controllers.ext

import play.api.mvc.Action
import play.api.mvc.AnyContent
import play.api.mvc.Request
import play.api.mvc.Result
import play.api.mvc.Security._

/**
 * @author andreas
 *
 */
trait Security {

  def isAuthenticated(f: => String => Request[AnyContent] => Result) = {
   Authenticated { user =>
     Action(request => f(user)(request))
   }
 }
}