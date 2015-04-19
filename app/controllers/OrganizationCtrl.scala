/**
 *
 */
package controllers

import controllers.ext.{ProvidesCtx, Security}
import models.{Address, Email, Homepage, Phone, Organization}
import play.api.data.Form
import play.api.data.Forms._
import play.api.Logger
import play.api.mvc._
import util.CustomFormatters
import play.api.i18n.Messages

import scalaz.{Failure, Success}

/**
 * Controller to handle requests for [[Organization]]s.
 *
 * @author andreas
 * @version 0.0.4, 2015-04-19
 */
object OrganizationCtrl extends Controller with ProvidesCtx with Security {

  implicit val charFormatter = CustomFormatters.charFormatter
  implicit val sqlDateFormatter = CustomFormatters.sqlDateFormatter

  val charMapping = of[Char]
  val sqlDateMapping = of[java.sql.Date]

  /**
   * Form to handle [[Organization]] data.
   */
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

  /**
   * Display an empty form to create a new [[Organization]].
   *
   * @return A response with an HTTP status code 200 and an empty form.
   */
  def create = isAuthorized("create.organization") { username =>
    implicit request =>
      Ok(views.html.orgForm(orgForm.bind(Map("gender" -> "m")).discardingErrors))
  }

  /**
   * Delete the [[Organization]] identified by the given <em>id</em>.
   *
   * @param id Identifier of the [[Organization]] to delete.
   * @return A redirect to display the list of [[Organization]]s.
   */
  def delete(id: Long) = isAuthorized("delete.organization") { username =>
    implicit request =>
      val result = Organization.delete(id)
      if (result.isSuccess) {
        Redirect(routes.OrganizationCtrl.list()).flashing("success" -> Messages("success.deleting.organization"))
      } else {
        Logger.logger.error(result.toString, result.toEither.left.get)
        Redirect(routes.OrganizationCtrl.show(id)).flashing("error" -> Messages("error.deleting.organization"))
      }
  }

  /**
   * Display a pre-filled form to edit the [[Organization]] identified by <em>id</em>
   * @param id Identifier of the [[Organization]] to modify.
   * @return
   */
  def edit(id: Long) = isAuthorized("edit.organization") { username =>
    implicit request =>
      val orgV = Organization.load(id)
      orgV match {
        case Success(orgOp) => orgOp match {
          case Some(org) => Ok(views.html.orgForm(orgForm.fill(org)))
          case None => Logger.error(s"Failed to load organization with ID '$id'. Does not exist.")
            Redirect(routes.OrganizationCtrl.list()).flashing("error" -> Messages("error.loading.organization"))
        }
        case Failure(t) => Logger.error(orgV.toString, t)
          Redirect(routes.OrganizationCtrl.list()).flashing("error" -> Messages("error.loading.organization"))
      }
  }

  /**
   * List all the [[Organization]]s stored.
   *
   * @return
   */
  def list = Action { implicit request =>
    val orgsV = Organization.getAll
    orgsV match {
      case Success(orgs) => Ok(views.html.orgList(orgs))
      case Failure(t) => Logger.error(orgsV.toString, t)
        Ok(views.html.orgList(List()))
    }
  }

  /**
   * Display the details of an [[Organization]] identified by <em>id</em>
   *
   * @param id Identifier of the [[Organization]] involved.
   * @return
   */
  def show(id: Long) = Action { implicit request =>
    val orgV = Organization.load(id)
    orgV match {
      case Success(orgOp) => orgOp match {
        case Some(org) => Logger.logger.debug(s"Found organization with ID '$id'.")
          val alist = Address.getOrgAddresses(org)
          val adrs = alist match {
            case Success(list) => list
            case Failure(_) => Nil
          }

          val plist = Phone.getOrgPhones(org)
          val phones = plist match {
            case Success(list) => list
            case Failure(_) => Nil
          }

          val elist = Email.getOrgEmails(org)
          val emails = elist match {
            case Success(list) => list
            case Failure(_) => Nil
          }

          val hpList = Homepage.getOrgHomepages(org)
          val hps = hpList match {
            case Success(list) => list
            case Failure(_) => Nil
          }

          Ok(views.html.org(org, adrs, phones, emails, hps))
        case None => Logger.error(s"Failed to load organization with ID '$id'. Does not exist.")
          Redirect(routes.OrganizationCtrl.list()).flashing("error" -> Messages("error.loading.organization"))
      }
      case Failure(t) => Logger.error(orgV.toString, t)
        Redirect(routes.OrganizationCtrl.list()).flashing("error" -> Messages("error.loading.organization"))
    }
  }

  /**
   * Store the submitted form data.
   *
   * @return
   */
  def submit = isAuthenticated { username =>
    implicit request =>
      orgForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the organization form.")
          BadRequest(views.html.orgForm(errors))
        },
        org => {
          Logger.debug(s"Storing organization $org.")
          val resultV = Organization.saveOrUpdate(org)
          resultV match {
            case Success(result) => Redirect(routes.OrganizationCtrl.show(resultV.toOption.get.id.get)).flashing(
              "success" -> Messages("success.succeededToStoreOrg"))
            case Failure(t) => Logger.error(resultV.toString, t)
            Redirect(routes.OrganizationCtrl.list()).flashing("error" -> Messages("error.storing.organization"))
          }
        })
  }
}