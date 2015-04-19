/**
 *
 */
package controllers

import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages
import play.api.Logger
import play.api.mvc._
import controllers.ext.{ProvidesCtx, Security}
import models.{Email, Organization, Person}
import util.{CustomFormatters, Privacy, UsageType}

import scalaz.{Failure, Success}

/**
 * Controller to handle [[Email]] requests.
 *
 * @author andreas
 * @version 0.0.8, 2015-04-19
 */
object EmailCtrl extends Controller with ProvidesCtx with Security {

  implicit val charFormatter = CustomFormatters.usageTypeFormatter
  val usageTypeMapping = of[UsageType]

  implicit val privacyFormatter = CustomFormatters.privacyFormatter
  val privacyMapping = of[Privacy]

  /**
   * Mapping for [[Email]] data.
   */
  val emailMapping = mapping(
    "id" -> optional(longNumber),
    "address" -> email,
    "created" -> longNumber,
    "creator" -> text,
    "modified" -> optional(longNumber),
    "modifier" -> optional(text))(Email.apply)(Email.unapply)

  /**
   * Form to handle [[Email]] data.
   */
  val emailForm = Form[(Email, UsageType, Privacy)](
    tuple("email" -> emailMapping,
      "usage" -> usageTypeMapping,
      "privacy" -> privacyMapping))

  /**
   * Form to handle [[Organization]] [[Email]] data.
   */
  val emailOrgForm = Form[Email](emailMapping)

  /**
   * Create a new [[Email]] for an [[Organization]] identified by <em>id</em>.
   *
   * @param oid The identifier of the [[Organization]] the [[Email]] relates to.
   * @return A response with HTTP status code 200 and an empty form for a payload.
   */
  def createOrgEmail(oid: Long) = isAuthenticated { username =>
    implicit request =>
      Ok(views.html.emailOrgForm(emailOrgForm, oid))
  }

  /**
   * Create a new [[Email]] for the [[Person]] identified by <em>pid</em>.
   *
   * @param pid The identifier of the [[Person]] the [[Email]] relates to.
   * @return A response with an HTTP status code 200 and an empty form for a payload.
   */
  def createPersonEmail(pid: Long) = isAuthenticated { username =>
    implicit request =>
      Ok(views.html.emailForm(emailForm.bind(Map("usage" -> "1", "privacy" -> "2")).discardingErrors, pid))
  }

  /**
   * Delete the [[Email]] identified by <em>id</em> related to the [[Organization]] identified by <em>oid</em>.
   *
   * @param oid Identifier of the [[Organization]] involved.
   * @param id Identifier of the [[Email]] to delete.
   * @return A redirect to display the details of the [[Organization]] identified by <em>oid</em>.
   */
  def deleteOrgEmail(oid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>
      val result = Email.deleteOrgEmail(oid, id)
      if (result.isSuccess) {
        Redirect(routes.OrganizationCtrl.show(oid)).flashing("success" -> Messages("success.deleting.email"))
      } else {
        Logger.logger.error(result.toString, result.toEither.left.get)
        Redirect(routes.OrganizationCtrl.show(oid)).flashing("error" -> Messages("error.deleting.email"))
      }
  }

  /**
   * Delete the [[Email]] identified by <em>id</em> related to the [[Person]] identified by <em>pid</em>.
   *
   * @param pid Identifier of the [[Person]] involved.
   * @param id Identifier of the [[Email]] to delete.
   * @return A redirect to display the details of the [[Person]] identified by <em>pid</em>.
   */
  def deletePersonEmail(pid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>
      val result = Email.deletePersonEmail(pid, id)
      if (result.isSuccess) {
        Redirect(routes.PersonCtrl.show(pid)).flashing("success" -> Messages("success.deleting.email"))
      } else {
        Logger.logger.error(result.toString, result.toEither.left.get)
        Redirect(routes.PersonCtrl.show(pid)).flashing("error" -> Messages("error.deleting.email"))
      }
  }

  /**
   * Modify an [[Organization]] [[Email]].
   *
   * @param oid Identifier of the [[Organization]] involved.
   * @param id Identifier of the [[Email]].
   * @return A response with HTTP status code 200 and a pre-filled form of the requested email. If the [[Email]] cannot
   *         be found a redirect to display the details of the [[Organization]] is returned.
   */
  def editOrgEmail(oid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>
      val orgV = Organization.load(oid)
      orgV match {
        case Success(orgOp) => if (orgOp.isDefined) {
          val emailOrgValidation = Email.getOrgEmail(orgOp.get, id)
          if (emailOrgValidation.isSuccess) {
            val orgEmailOp = emailOrgValidation.toOption.get
            if (orgEmailOp.isDefined) {
              Ok(views.html.emailOrgForm(emailOrgForm.fill(orgEmailOp.get._1), oid))
            } else {
              Logger.error(s"Failed to load email with ID '$id' for organization with ID '$oid'. Not related.")
              Redirect(routes.OrganizationCtrl.show(oid)).flashing("error" -> Messages("error.loading.email"))
            }
          } else {
            Logger.error(s"Failed to load email with ID '$id' for organization with ID '$oid'.")
            Redirect(routes.OrganizationCtrl.show(oid)).flashing("error" -> Messages("error.loading.organization.email"))
          }
        } else {
          Logger.error("Failed to load organization with ID '$oid'.")
          Redirect(routes.OrganizationCtrl.list()).flashing("error" -> Messages("error.loading.organization"))
        }
        case Failure(t) => Logger.error(orgV.toString, t)
          Redirect(routes.OrganizationCtrl.list()).flashing("error" -> Messages("error.loading.organization"))
      }
  }

  /**
   * Modify a [[Person]] [[Email]].
   *
   * @param pid Identifier of the [[Person]] involved.
   * @param id Identifier of the [[Email]].
   * @return A response with HTTP status code 200 and a pre-filled form of the requested email. If the [[Email]] cannot
   *         be found a redirect to display the details of the [[Person]] is returned.
   */
  def editPersonEmail(pid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>
      val personV = Person.load(pid)
      if (personV.isSuccess) {
        val personOp = personV.toOption.get
        if (personOp.isDefined) {
          val emailPersonValidation = Email.getPersonEmail(personOp.get, id)
          if (emailPersonValidation.isSuccess) {
            val personEmailOp = emailPersonValidation.toOption.get
            if (personEmailOp.isDefined) {
              Ok(views.html.emailForm(emailForm.fill((personEmailOp.get._1, personEmailOp.get._2.usage, personEmailOp.get._2.privacy)), pid))
            } else {
              Logger.error(s"Failed to load email with ID '$id' for person with ID '$pid'. Not related.")
              Redirect(routes.PersonCtrl.show(pid)).flashing("error" -> Messages("error.loading.email"))
            }
          } else {
            Logger.error(s"Failed to load email with ID '$id' for person with ID '$pid'.")
            Redirect(routes.PersonCtrl.show(pid)).flashing("error" -> Messages("error.loading.person.email"))
          }
        } else {
          Logger.error(s"Failed to load person with ID '$pid'.")
          Redirect(routes.PersonCtrl.list()).flashing("error" -> Messages("error.loading.person"))
        }
      } else {
        Logger.error(personV.toString, personV.toEither.left.get)
        Redirect(routes.PersonCtrl.list()).flashing("error" -> Messages("error.loading.person"))
      }
  }

  /**
   * Show the [[Email]]s associated with the [[Organization]] identified by <em>oid</em>.
   *
   * @param oid Identifier of the [[Organization]].
   * @return A response with an HTTP status code 200 and the list of [[Email]]s for the specific [[Organization]]. If
   *         the [[Email]]s cannot be loaded a redirect to display the details of the [[Organization]] is returned.
   *         If the [[Organization]] cannot be loaded a redirect to display the list of [[Organization]]s is returned.
   */
  def showOrgEmail(oid: Long) = isAuthenticated { username =>
    implicit request =>
      val orgV = Organization.load(oid)
      orgV match {
        case Success(orgOp) => if (orgOp.isDefined) {
          val orgEmails = Email.getOrgEmails(orgOp.get)
          if (orgEmails.isSuccess) {
            Ok(views.html.emailOrg(orgOp.get, orgEmails.toOption.get))
          } else {
            Logger.error(s"Failed to load email addresses for organization '${orgOp.get.name}'")
            Redirect(routes.OrganizationCtrl.show(oid)).flashing("error" -> Messages("error.loading.organization.emails"))
          }
        } else {
          Logger.error(s"Failed to load organization with ID '$oid'")
          Redirect(routes.OrganizationCtrl.list()).flashing("error" -> Messages("error.loading.organization"))
        }
        case Failure(t) => Logger.error(orgV.toString, t)
          Redirect(routes.OrganizationCtrl.list()).flashing("error" -> Messages("error.loading.organization"))
      }
  }

  /**
   * Show the [[Email]]s associated with the [[Person]] identified by <em>pid</em>.
   *
   * @param pid Identifier of the [[Person]].
   * @return A response with an HTTP status code 200 and the list of [[Email]]s for the specific [[Person]]. If
   *         the [[Email]]s cannot be loaded a redirect to display the details of the [[Person]] is returned.
   *         If the [[Person]] cannot be loaded a redirect to display the list of [[Person]]s is returned.
   */
  def showPersonEmail(pid: Long) = isAuthenticated { username =>
    implicit request =>
      val personV = Person.load(pid)
      if (personV.isSuccess) {
        val personOp = personV.toOption.get
        if (personOp.isDefined) {
          val persEmails = Email.getPersonEmails(personOp.get)
          if (persEmails.isSuccess) {
            Ok(views.html.email(personOp.get, persEmails.toOption.get))
          } else {
            Logger.error(s"Failed to load email addresses for person '${personOp.get.fullname}'")
            Redirect(routes.PersonCtrl.show(pid)).flashing("error" -> Messages("error.loading.person.emails"))
          }
        } else {
          Logger.error(s"Failed to load person with ID '$pid'. Does not exist.")
          Redirect(routes.PersonCtrl.list()).flashing("error" -> Messages("error.loading.person"))
        }
      } else {
        Logger.error(personV.toString, personV.toEither.left.get)
        Redirect(routes.PersonCtrl.list()).flashing("error" -> Messages("error.loading.person"))
      }
  }

  /**
   * Submit the form data.
   *
   * @param oid Identifier of the [[Organization]] the [[Email]] relates to.
   * @return A redirect to display the [[Email]] of the [[Organization]] identified by <em>oid</em>. If form
   *         validation or storing the [[Email]] fails a response with HTTP status code 400 is generated and the form
   *         returned with error information. If the [[Organization]] cannot be loaded a redirect to display the list
   *         of [[Organization]]s is returned.
   */
  def submitOrgEmail(oid: Long) = isAuthenticated { username =>
    implicit request =>
      emailOrgForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the organization email form. Hint: " + errors.error("address").toString)
          BadRequest(views.html.emailOrgForm(errors, oid))
        },
        email => {
          val orgV = Organization.load(oid)
          orgV match {
            case Success(orgOp) => if (orgOp.isDefined) {
              Logger.debug(s"Storing organization email '${email.address}' for organization '${orgOp.get.name}'.")
              val result = Email.saveOrgEmail(orgOp.get, email)
              if (result.isSuccess) {
                Redirect(routes.EmailCtrl.showOrgEmail(oid)).flashing("success" -> Messages("success.storing" +
                  ".organization.email"))
              } else {
                Logger.error(result.toString, result.toEither.left.get)
                BadRequest(views.html.emailOrgForm(emailOrgForm, oid)).flashing("error" -> Messages("error.storing" +
                  ".organization.email"))
              }
            } else {
              Redirect(routes.OrganizationCtrl.list()).flashing("error" -> Messages("error.loading.organization"))
            }
            case Failure(t) => Logger.error(orgV.toString, t)
              Redirect(routes.OrganizationCtrl.list()).flashing("error" -> Messages("error.loading.organization"))
          }
        })
  }

  /**
   * Submit the form data.
   *
   * @param pid Identifier of the [[Person]] involved.
   * @return A redirect to display the [[Email]] of the [[Person]] identified by <em>pid</em>. If form
   *         validation or storing the [[Email]] fails a response with HTTP status code 400 is generated and the form
   *         returned with error information. If the [[Person]] cannot be loaded a redirect to display the list
   *         of [[Person]]s is returned.
   */
  def submitPersonEmail(pid: Long) = isAuthenticated { username =>
    implicit request =>
      emailForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the email form.")
          BadRequest(views.html.emailForm(errors, pid))
        },
        email => {
          val personV = Person.load(pid)
          if (personV.isSuccess) {
            val personOp = personV.toOption.get
            if (personOp.isDefined) {
              Logger.debug(s"Storing person email '${email._1.address}' for person '${personOp.get.fullname}'")
              val result = Email.savePersonEmail(personOp.get, email._1, email._2, email._3)
              if (result.isSuccess) {
                Redirect(routes.EmailCtrl.showPersonEmail(pid)).flashing("success" -> Messages("success.storing.person" +
                  ".email"))
              } else {
                Logger.error(result.toString, result.toEither.left.get)
                BadRequest(views.html.emailForm(emailForm, pid)).flashing("error" -> Messages("error.storing" +
                  ".person.email"))
              }
            } else {
              Logger.error(s"Failed to load person with ID '$pid'.")
              Redirect(routes.PersonCtrl.list()).flashing("error" -> Messages("error.loading.person"))
            }
            val p = personOp.get
            Logger.debug(s"Storing email address '$email._1.address' for person '${p.fullname}'")
            val result = Email.savePersonEmail(p, email._1, email._2, email._3)
            if (result.isSuccess) {
              Redirect(routes.EmailCtrl.showPersonEmail(pid)).flashing("success" -> Messages("success.storing.email"))
            } else {
              Logger.error(result.toString, result.toEither.left.get)
              BadRequest(views.html.emailForm(emailForm, pid)).flashing("error" -> Messages("error.storing.email"))
            }
          } else {
            Logger.error(personV.toString, personV.toEither.left.get)
            Redirect(routes.PersonCtrl.list()).flashing("error" -> Messages("error.loading.person"))
          }
        })
  }
}