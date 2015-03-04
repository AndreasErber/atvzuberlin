/**
 *
 */
package controllers

import controllers.ext.ProvidesCtx
import controllers.ext.Security
import models.{ Organization, Organizations }
import play.api.data.Form
import play.api.data.Forms._
import play.api.db._
import play.api.Logger
import play.api.mvc._
import play.api.mvc.Security._
import play.api.Play.current
import util.CustomFormatters
import views.html.defaultpages.todo
import views.html.defaultpages.notFound
import play.api.i18n.Messages
import models.Address
import models.Phone
import models.Email
import models.Homepage

/**
 * @author andreas
 * @version 0.0.3, 2015-01-03
 */
object OrganizationCtrl extends Controller with ProvidesCtx with Security {

  implicit val charFormatter = CustomFormatters.charFormatter
  implicit val sqlDateFormatter = CustomFormatters.sqlDateFormatter

  val charMapping = of[Char]
  val sqlDateMapping = of[java.sql.Date]

  val orgForm = Form(
    mapping("id" -> optional(longNumber),
      "name" -> nonEmptyText,
      "gender" -> charMapping,
      "founded" -> optional(sqlDateMapping),
      "refounded" -> optional(sqlDateMapping),
      "motto" -> optional(text),
      "colors" -> optional(text),
      "city" -> optional(text),
      "created" -> longNumber,
      "creator" -> text,
      "modified" -> optional(longNumber),
      "modifier" -> optional(text))(Organization.apply)(Organization.unapply))

  def create = isAuthenticated { username =>
    implicit request =>
      Ok(views.html.orgForm(orgForm.bind(Map("gender" -> "m")).discardingErrors))
  }

  def delete(id: Long) = isAuthenticated { username =>
    implicit request =>
      val result = Organization.delete(id)
      if (result.isSuccess) {
        Redirect(routes.OrganizationCtrl.list).flashing(("success" -> Messages("success.succeededToDeleteOrg")))
      } else {
        Logger.logger.error(result.toString(), result.toEither.left.get)
        Redirect(routes.OrganizationCtrl.show(id)).flashing(("error" -> Messages("error.failedToDeleteOrg")))
      }
  }

  def edit(id: Long) = isAuthenticated { username =>
    implicit request =>
      val o = Organization.load(id)
      o match {
        case None =>
          Logger.logger.debug("Cannot find organization with ID " + id + "."); NotFound
        case Some(org) =>
          Logger.logger.debug("Preparing editing of organization with ID " + id + ".");
          Ok(views.html.orgForm(orgForm.fill(org)))
        case _ => NotFound
      }
  }

  def list = Action { implicit request =>
    val result = Organization.getAll
    if (result.isSuccess) {
      Ok(views.html.orgList(result.toOption.get))
    } else {
      Logger.error(result.toString(), result.toEither.left.get)
      Ok(views.html.orgList(List()))
    }
  }

  def show(id: Long) = Action { implicit request =>
    val o = Organization.load(id)
    o match {
      case None =>
        Logger.logger.debug("No organization with ID " + id + " found."); NotFound
      case Some(org) =>
        Logger.logger.debug("Found organization with ID " + id + ".")
        val alist = Address.getOrgAddresses(org)
        val adrs = if (alist.isSuccess) alist.toOption.get else Nil
        val plist = Phone.getOrgPhones(org)
        val phones = if (plist.isSuccess) plist.toOption.get else Nil
        val elist = Email.getOrgEmails(org)
        val emails = if (elist.isSuccess) elist.toOption.get else Nil
        val hpList = Homepage.getOrgHomepages(org)
        val hps = if (hpList.isSuccess) hpList.toOption.get else Nil
        Ok(views.html.org(org, adrs, phones, emails, hps))
      case _ => NotFound
    }
  }

  def submit = isAuthenticated { username =>
    implicit request =>
      orgForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the organization form.")
          BadRequest(views.html.orgForm(errors))
        },
        org => {
          Logger.debug("Storing organization " + org)
          val result = Organization.saveOrUpdate(org)
          if (result.isSuccess) {
            Redirect(routes.OrganizationCtrl.show(result.toOption.get.id.get)).flashing(("success" -> Messages("success.succeededToStoreOrg")))
          } else {
            Logger.error(result.toString(), result.toEither.left.get)
            BadRequest(views.html.orgForm(orgForm)).flashing("error" -> Messages("error.failedToStoreOrg"))
          }
        })
  }
}