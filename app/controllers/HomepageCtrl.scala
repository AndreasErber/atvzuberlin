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
import models.{ Homepage, Homepages }
import play.api.Play.current
import models.Person
import models.PersonHasHomepage
import controllers.ext.ProvidesCtx
import controllers.ext.Security
import play.api.i18n.Messages
import models.Organization


/**
 * @author andreas
 * @version 0.0.1, 2013-04-12
 */
object HomepageCtrl extends Controller with ProvidesCtx with Security {

  implicit val usageFormatter = CustomFormatters.usageTypeFormatter
  val usageMapping = of[UsageType]

  val hpMapping = mapping(
    "id" -> optional(longNumber),
    "url" -> nonEmptyText,
    "descr" -> optional(text),
    "created" -> longNumber,
    "creator" -> text,
    "modified" -> optional(longNumber),
    "modifier" -> optional(text))(Homepage.apply)(Homepage.unapply)

   val hpPersForm = Form[(Homepage, UsageType)](
    tuple(
      "homepage" -> hpMapping,
      "usage" -> usageMapping))
      
    val hpOrgForm = Form[Homepage](hpMapping)
    
   def createOrgHomepage(oid: Long) = isAuthenticated { username =>
    implicit request =>
      Ok(views.html.homepageOrgForm(hpOrgForm, oid))
  }

  def createPersonHomepage(pid: Long) = isAuthenticated { username =>
    implicit request =>
      Ok(views.html.homepageForm(hpPersForm.bind(Map("usage" -> "1", "privacy" -> "2")).discardingErrors, pid))
  }

  def deleteOrgHomepage(oid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>
      val result = Homepage.deleteOrgHomepage(oid, id)
      if (result.isSuccess) {
        Redirect(routes.HomepageCtrl.showOrgHomepage(oid)).flashing(("success" -> Messages("success.succeededToDeleteHomepage")))
      } else {
        Logger.logger.error(result.toString(), result.fail.toOption.get)
        Redirect(routes.HomepageCtrl.showOrgHomepage(oid)).flashing(("error" -> Messages("error.failedToDeleteHomepage")))
      }
  }

  def deletePersonHomepage(pid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>
      val result = Homepage.deletePersonHomepage(pid, id)
      if (result.isSuccess) {
        Redirect(routes.HomepageCtrl.showPersonHomepage(pid)).flashing(("success" -> Messages("success.succeededToDeleteHomepage")))
      } else {
        Logger.logger.error(result.toString(), result.fail.toOption.get)
        Redirect(routes.HomepageCtrl.showPersonHomepage(pid)).flashing(("error" -> Messages("error.failedToDeleteHomepage")))
      }
  }

  def editOrgHomepage(oid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>
      Ok(views.html.homepageOrgForm(hpOrgForm.fill(Homepage.load(id).get), oid))
  }

  def editPersonHomepage(pid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>
      val result = Homepage.getPersonHomepage(Person.load(pid).get, id)
      if (result.isSuccess) {
        val x = result.toOption.get
        if (x.isDefined) {
          Ok(views.html.homepageForm(hpPersForm.fill((x.get._1, x.get._2.usage)), pid))
        } else {
          Redirect(routes.HomepageCtrl.showPersonHomepage(pid)).flashing("error" -> Messages("error.failedToLoadHomepage"))
        }
      } else {
        Redirect(routes.HomepageCtrl.showPersonHomepage(pid)).flashing("error" -> Messages("error.failedToLoadHomepage"))
      }
  }

  def showOrgHomepage(oid: Long) = isAuthenticated { username =>
    implicit request =>
      val o = Organization.load(oid).get
      val req = Ok(views.html.homepageOrg(o, Homepage.getOrgHomepages(o).toOption.get))
      if (flash.get("error").isDefined) {
        req.flashing(("error" -> flash.get("error").get))
      } else if (flash.get("success").isDefined) {
        req.flashing(("success" -> flash.get("success").get))
      } else {
        req
      }
  }

  def showPersonHomepage(pid: Long) = isAuthenticated { username =>
    implicit request =>
      val p = Person.load(pid).get
      val req = Ok(views.html.homepage(p, Homepage.getPersonHomepages(p).toOption.get))
      if (flash.get("error").isDefined) {
        req.flashing(("error" -> flash.get("error").get))
      } else if (flash.get("success").isDefined) {
        req.flashing(("success" -> flash.get("success").get))
      } else {
        req
      }
  }

  def submitOrgHomepage(oid: Long) = isAuthenticated { username =>
    implicit request =>
      hpOrgForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the email form.")
          BadRequest(views.html.homepageOrgForm(errors, oid))
        },
        hp => {
          val o = Organization.load(oid).get
          Logger.debug("Storing homwepage " + hp.url + " for organization " + o.name)
          val result = Homepage.saveOrgHomepage(o, hp)
          if (result.isSuccess) {
            Redirect(routes.HomepageCtrl.showOrgHomepage(oid)).flashing(("success" -> Messages("success.succeededToStoreHomepage")))
          } else {
            Logger.error(result.toString(), result.fail.toOption.get)
            BadRequest(views.html.homepageOrgForm(hpOrgForm, oid)).flashing("error" -> Messages("error.failedToStoreHomepage"))
          }
        })
  }

  def submitPersonHomepage(pid: Long) = isAuthenticated { username =>
    implicit request =>
      hpPersForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the email form.")
          BadRequest(views.html.homepageForm(errors, pid))
        },
        hp => {
          val p = Person.load(pid).get
          Logger.debug("Storing homepage " + hp._1.url + " for person " + p.lastname + ", " + p.firstname.getOrElse(""))
          val result = Homepage.savePersonHomepage(p, hp._1, hp._2)
          if (result.isSuccess) {
            Redirect(routes.HomepageCtrl.showPersonHomepage(pid)).flashing(("success" -> Messages("success.succeededToStoreHomepage")))
          } else {
            Logger.error(result.toString(), result.fail.toOption.get)
            BadRequest(views.html.homepageForm(hpPersForm, pid)).flashing("error" -> Messages("error.failedToStoreHomepage"))
          }
        })
  }
}