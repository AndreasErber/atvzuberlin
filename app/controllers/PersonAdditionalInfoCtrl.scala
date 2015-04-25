/**
 *
 */
package controllers

import util.{CustomFormatters, MemberState}
import play.api.data.Form
import play.api.data.Forms._
import play.api.Logger
import play.api.mvc._
import controllers.ext.{ProvidesCtx, Security}
import util.{FormOfAddress, LetterSalutation}
import models.PersonAdditionalInfo
import play.api.i18n.Messages
import scalaz.{Failure, Success}

/**
 * Controller to handle requests for additional person information.
 *
 * @author andreas
 * @version 0.0.4, 2015-04-25
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

  /**
   * Display an empty form.
   *
   * @param pid Identifier of the person involved.
   * @return
   */
  def create(pid: Long) = isAuthenticated { username =>
    implicit request =>
      Ok(views.html.personAdditionalForm(paiForm.bind(Map("id" -> pid.toString))))
  }

  /**
   * Display a pre-filled form.
   *
   * @param pid Identifier of the person.
   * @return
   */
  def edit(pid: Long) = isAuthenticated { username =>
    implicit request =>
      val paiV = PersonAdditionalInfo.load(pid)
      paiV match {
        case Success(paiOp) => paiOp match {
          case Some(pai) =>
            Ok(views.html.personAdditionalForm(paiForm.fill(pai)))
          case None =>
            Logger.debug(s"Failed to load person additional info with ID '$pid'. Does not exist.")
            Redirect(routes.PersonCtrl.show(pid)).flashing("error" -> Messages("error.loading.personAI"))
        }
        case Failure(t) => Logger.error(paiV.toString, t)
          Redirect(routes.PersonCtrl.show(pid)).flashing("error" -> Messages("error.loading.personAI"))
      }
  }

  /**
   * Handle submitted form data.
   *
   * @return
   */
  def submit = isAuthenticated { username =>
    implicit request =>
      paiForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the personal additional information form.")
          BadRequest(views.html.personAdditionalForm(errors))
        },
        p => {
          val resultV = PersonAdditionalInfo.saveOrUpdate(p)
          resultV match {
            case Success(result) => Redirect(routes.PersonCtrl.show(result.id.get)).flashing("success" -> Messages
              ("error.storing.personAI"))
            case Failure(t) => Logger.error(resultV.toString, t)
              Redirect(routes.PersonCtrl.list()).flashing("error" -> Messages("error.storing.personAI"))
          }
        })
  }
}