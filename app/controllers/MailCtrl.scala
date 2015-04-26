package controllers

import controllers.ext.ProvidesCtx
import models.{PersonInCharges, Person, Charge, Charges}
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages
import play.api.Logger
import play.api.mvc.{Action, Controller}
import play.api.Play.current

import scalaz.{Failure, Success}

/**
 * Controller to handle the mail form and the mail sending.
 *
 * @author andreas
 * @version 0.0.1, 2015-04-26
 */
object MailCtrl extends Controller with ProvidesCtx {

  val mailForm = Form(
    tuple(
      "from" -> nonEmptyText,
      "to" -> longNumber,
      "subject" -> nonEmptyText,
      "content" -> nonEmptyText
    )
  )

  def create = Action { implicit request =>
    val chargesV = Charge.getAll
    chargesV match {
      case Success(charges) => Ok(views.html.mailForm(mailForm, charges))
      case Failure(t) => Logger.error(chargesV.toString, t)
        Redirect(routes.UserCtrl.index()).flashing("error" -> Messages("error.loading.charges.for.mail"))
    }

  }

  def submit = Action { implicit request =>
    mailForm.bindFromRequest().fold(
      formWithErrors => {
        Logger.error("Registration failed due to: " + formWithErrors.errors.mkString(","))
        BadRequest(views.html.mailForm(formWithErrors, Nil))
      },
      tuple => {
        val chargeV = Charge.load(tuple._2)
        chargeV match {
          case Success(chargeOp) => chargeOp match {
            case Some(charge) => val personV = PersonInCharges.getPersonOfCharge(charge)
              personV match {
                case Success(personOp) => personOp match {
                  case Some(person) => sendMail(tuple._1, person, charge.emailMale.get, tuple._3, tuple._4)
                  case None => Logger.error(s"Failed to load current person for charge '${charge.nameMale}'. Does not" +
                    s" exist.")
                    // TODO we require a default target for the mail here
                    Redirect(routes.UserCtrl.index()).flashing("error" -> "error.loading.person.in.charge.for.mail")
                }
                case Failure(t) => Logger.error(personV.toString, t)
                  // TODO we require a default target for the mail here
                  Redirect(routes.UserCtrl.index()).flashing("error" -> "error.loading.person.in.charge.for.mail")
              }
            case None => Logger.error(s"Failed to load charge with ID '${tuple._2}. Does not exist.")
              Redirect(routes.UserCtrl.index()).flashing("error" -> Messages("error.loading.charge.for.mail"))
          }
          case Failure(t) => Logger.error(chargeV.toString, t)
            Redirect(routes.UserCtrl.index()).flashing("error" -> Messages("error.loading.charge.for.mail"))
        }
        BadRequest
      }
    )
  }

  private def sendMail(from: String, toPerson: Person, toMail: String, subject: String, body: String) = {
    var mailBodyHead = "Liebe"
    if (toPerson.gender == 'm') {
      mailBodyHead += "r"
    }
    val mailBody =
      s"""$mailBodyHead ${toPerson.nickname},
         |
         |über das Mailformular der Webseite des ATV zu Berlin wurde dir folgende Mail geschickt:
         |
         |
         |$body
       """.stripMargin
    val emailMsg = play.api.libs.mailer.Email(subject, "website@atvzuberlin.de", Seq(toMail), bodyText = Some(mailBody))
    Logger.debug("Sending email:")
    Logger.debug(emailMsg.toString)
    play.api.libs.mailer.MailerPlugin.send(emailMsg)
  }
}
