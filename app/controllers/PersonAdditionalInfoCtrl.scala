/**
 *
 */
package controllers

import util.CustomFormatters
import play.api.data.Form
import play.api.data.Forms._
import play.api.db._
import play.api.Logger
import play.api.mvc._
import play.api.mvc.Security._
import play.api.Play.current
import models.Email
import controllers.ext.{ ProvidesCtx, Security }
import util.{ FormOfAddress, LetterSalutation }
import models.PersonAdditionalInfo
import play.api.i18n.Messages
import util.MemberState

/**
 * @author andreas
 * @version 0.0.2, 2015-01-03
 */
object PersonAdditionalInfoCtrl extends Controller with ProvidesCtx with Security {

  implicit val formOfAddressFormatter = CustomFormatters.formOfAddressFormatter
  implicit val letterSalutationFormatter = CustomFormatters.letterSalutationFormatter
  implicit val memberStateFormatter = CustomFormatters.memberStateFormatter
  implicit val sqlDateFormatter = CustomFormatters.sqlDateFormatter

  val formOfAddressMapping = of[FormOfAddress]
  val letterSalutationMapping = of[LetterSalutation]
  val personStatusMapping = of[MemberState]
  val sqlDateMapping = of[java.sql.Date]

  val paiForm = Form[PersonAdditionalInfo](
    mapping(
      "id" -> longNumber,
      "formOfAddress" -> formOfAddressMapping,
      "letterSalutation" -> letterSalutationMapping,
      "enlistment" -> optional(sqlDateMapping),
      "withdrawal" -> optional(sqlDateMapping),
      "status" -> personStatusMapping,
      "profession" -> optional(text),
      "employer" -> optional(text),
      "created" -> longNumber,
      "creator" -> text,
      "modified" -> optional(longNumber),
      "modifier" -> optional(text))(PersonAdditionalInfo.apply)(PersonAdditionalInfo.unapply))
      
  def create(pid: Long) = isAuthenticated { username =>
    implicit request =>
      Ok(views.html.personAdditionalForm(paiForm.bind(Map("id" -> pid.toString))))
  }
  
  def edit(pid: Long) = isAuthenticated { username =>
    implicit request =>
      val pai = PersonAdditionalInfo.load(pid)
      pai match {
        case None =>
          Logger.debug("Cannot find person additional info with ID " + pid + ".")
          Redirect(routes.PersonCtrl.show(pid)).flashing("error" -> Messages("error.failedToLoadPersonAI", pid))
        case Some(persAI) =>
          Logger.debug("Preparing editing of person additional info with ID " + pid + ".");
          Ok(views.html.personAdditionalForm(paiForm.fill(persAI)))
        case _ => Redirect(routes.PersonCtrl.show(pid)).flashing("error" -> Messages("error.failedToLoadPersonAI", pid))
      }
  }
  
  def submit = isAuthenticated { username =>
    implicit request =>
      paiForm.bindFromRequest.value map {
        p =>
          val result = PersonAdditionalInfo.saveOrUpdate(p)
          if (result.isSuccess) {
            Redirect(routes.PersonCtrl.show(result.toOption.get.id.get)).flashing("success" -> Messages("success.succeededToStorePersonAI", result.toOption.get.id.get))
          } else {
            Logger.logger.error(result.toString(), result.toEither.left.get)
            Redirect(routes.PersonCtrl.list).flashing("error" -> Messages("error.failedToStorePersonAI", paiForm.data.get("id").get))
          }

      } getOrElse BadRequest
  }
}