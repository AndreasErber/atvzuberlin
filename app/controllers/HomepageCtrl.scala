/**
 *
 */
package controllers

import play.api.data.Form
import play.api.data.Forms._
import play.api.Logger
import play.api.mvc._
import util.{CustomFormatters, UsageType}
import models.{Homepage, Organization, Person}
import controllers.ext.{ProvidesCtx, Security}
import play.api.i18n.Messages

import scalaz.{Failure, Success}

/**
 * Controller for handling [[Homepage]] related requests.
 *
 * @author andreas
 * @version 0.0.4, 2015-04-19
 */
object HomepageCtrl extends Controller with ProvidesCtx with Security {

  implicit val usageFormatter = CustomFormatters.usageTypeFormatter
  val usageMapping = of[UsageType]

  /**
   * A mapping for form data for [[Homepage]]s.
   */
  val hpMapping = mapping(
    "id" -> optional(longNumber),
    "url" -> nonEmptyText,
    "descr" -> optional(text),
    "created" -> longNumber,
    "creator" -> text,
    "modified" -> optional(longNumber),
    "modifier" -> optional(text))(Homepage.apply)(Homepage.unapply)

  /**
   * A form for [[Person]]al [[Homepage]]s.
   */
  val hpPersForm = Form[(Homepage, UsageType)](
    tuple(
      "homepage" -> hpMapping,
      "usage" -> usageMapping))

  /**
   * A form for [[Organization]] [[Homepage]]s.
   */
  val hpOrgForm = Form[Homepage](hpMapping)

  /**
   * Display an empty form to create an [[Organization]] [[Homepage]]
   *
   * @param oid Identifier of the [[Organization]] involved.
   * @return A response with an HTTP code 200 and an empty form.
   */
  def createOrgHomepage(oid: Long) = isAuthorized("create.organization.homepage") { username =>
    implicit request =>
      Ok(views.html.homepageOrgForm(hpOrgForm, oid))
  }

  /**
   * Display an empty form to create a [[Person]]al [[Homepage]].
   *
   * @param pid Identifier of the [[Person]] involved.
   * @return A response with an HTTP status code 200 and an empty form.
   */
  def createPersonHomepage(pid: Long) = isAuthorized("create.person.homepage") { username =>
    implicit request =>
      Ok(views.html.homepageForm(hpPersForm.bind(Map("usage" -> "1", "privacy" -> "2")).discardingErrors, pid))
  }

  /**
   * Delete the [[Organization]] [[Homepage]] identified by <em>id</em>.
   *
   * @param oid Identifier of the [[Organization]] involved.
   * @param id Identifier of the [[Homepage]] to delete.
   * @return
   */
  def deleteOrgHomepage(oid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>
      val resultV = Homepage.deleteOrgHomepage(oid, id)
      resultV match {
        case Success(result) => if (result) {
          Redirect(routes.OrganizationCtrl.show(oid)).flashing("success" -> Messages("success.deleting.homepage"))
        } else {
          Logger.error(s"No homepage with ID '$id' for organization with ID '$oid' was deleted.")
          Redirect(routes.OrganizationCtrl.show(oid)).flashing("success" -> Messages("error.deleting.homepage"))
        }
        case Failure(t) => Logger.logger.error(resultV.toString, t)
          Redirect(routes.OrganizationCtrl.show(oid)).flashing("error" -> Messages("error.deleting.homepage"))
      }
  }

  /**
   * Delete the [[Person]]al [[Homepage]] identified by <em>id</em>.
   *
   * @param pid Identifier of the [[Person]] involved.
   * @param id Identifier of the [[Homepage]] to be deleted.
   * @return
   */
  def deletePersonHomepage(pid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>
      val resultV = Homepage.deletePersonHomepage(pid, id)
      resultV match {
        case Success(result) => if (result) {
          Redirect(routes.PersonCtrl.show(pid)).flashing("success" -> Messages("success.deleting.homepage"))
        } else {
          Logger.error(s"No homepage with ID '$id' for person with ID '$pid' was deleted.")
          Redirect(routes.PersonCtrl.show(pid)).flashing("error" -> Messages("error.deleting.homepage"))
        }
        case Failure(t) => Logger.logger.error(resultV.toString, resultV.toEither.left.get)
          Redirect(routes.PersonCtrl.show(pid)).flashing("error" -> Messages("error.deleting.homepage"))
      }
  }

  /**
   * Display a pre-filled form for editing an [[Organization]] [[Homepage]].
   *
   * @param oid Identifier of the [[Organization]] involved.
   * @param id Identifier of the [[Homepage]] to modify.
   * @return
   */
  def editOrgHomepage(oid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>
      val organizationV = Organization.load(oid)
      organizationV match {
        case Success(organizationOp) => organizationOp match {
          case Some(organization) => val orgHasHomepageV = Homepage.getOrgHomepage(organization, id)
            orgHasHomepageV match {
              case Success(pairOp) => pairOp match {
                case Some(pair) => Ok(views.html.homepageOrgForm(hpOrgForm.fill(pair._1), oid))
                case None => Logger.error(s"Failed to load homepage with ID '$id'. Does not exist.")
                  Redirect(routes.OrganizationCtrl.show(oid)).flashing("error" -> Messages("error.loading.organization" +
                    ".homepage"))
              }
              case Failure(t) => Logger.error(orgHasHomepageV.toString, t)
                Redirect(routes.OrganizationCtrl.show(oid)).flashing("error" -> Messages("error.loading.organization" +
                  ".homepage"))
            }
          case None => Logger.error(s"Failed to load organization with ID '$oid'. Does not exist.")
            Redirect(routes.OrganizationCtrl.list()).flashing("error" -> Messages("error.loading.organization"))
        }
        case Failure(t) => Logger.error(organizationV.toString, t)
          Redirect(routes.OrganizationCtrl.list()).flashing("error" -> Messages("error.loading.organization"))
      }
  }

  /**
   * Display a pre-filled form for editing a [[Person]]al [[Homepage]].
   *
   * @param pid Identifier of the [[Person]] involved.
   * @param id Identifier of the [[Homepage]] to edit.
   * @return
   */
  def editPersonHomepage(pid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>
      val personV = Person.load(pid)
      personV match {
        case Success(personOp) => val personOp = personV.toOption.get
          personOp match {
            case Some(person) => val persHasHomepageV = Homepage.getPersonHomepage(person, id)
              persHasHomepageV match {
                case Success(persHasHomepageOp) => persHasHomepageOp match {
                  case Some(persHasHomepage) => Ok(views.html.homepageForm(hpPersForm.fill((persHasHomepage._1,
                    persHasHomepage._2.usage)), pid))
                  case None => Logger.error(s"Failed to load homepage with ID '$id'. Does not exist.")
                    Redirect(routes.PersonCtrl.show(pid)).flashing("error" -> Messages("error" +
                      ".loading.person.homepage"))
                }
                case Failure(t) => Logger.error(persHasHomepageV.toString, persHasHomepageV.toEither.left.get)
                  Redirect(routes.PersonCtrl.show(pid)).flashing("error" -> Messages("error.loading.person.homepage"))
              }
            case None => Logger.error(s"Failed to load person with ID '$pid'. Does not exist.")
              Redirect(routes.PersonCtrl.list()).flashing("error" -> Messages("error.loading.person"))
          }
        case Failure(t) => Logger.error(personV.toString, t)
          Redirect(routes.PersonCtrl.list()).flashing("error" -> Messages("error.loading.person"))
      }
  }

  /**
   * Display the details of an [[Organization]] [[Homepage]].
   *
   * @param oid Identifier of the [[Organization]] involved.
   * @return
   */
  def showOrgHomepage(oid: Long) = isAuthenticated { username =>
    implicit request =>
      val orgV = Organization.load(oid)
      orgV match {
        case Success(orgOp) => orgOp match {
          case Some(org) => val orgHasHomepageV = Homepage.getOrgHomepages(org)
            orgHasHomepageV match {
              case Success(orgHasHomepages) => Ok(views.html.homepageOrg(org, orgHasHomepages))
              case Failure(t) => Logger.error(orgHasHomepageV.toString, t)
                Redirect(routes.OrganizationCtrl.show(oid)).flashing("error" -> Messages("error.loading.organization" +
                  ".homepages"))
            }
          case None => Logger.error(s"Failed to load organization with ID '$oid'. Does not exist.")
            Redirect(routes.OrganizationCtrl.list()).flashing("error"->Messages("error.loading.organization"))
        }
        case Failure(t) => Logger.error(orgV.toString, t)
          Redirect(routes.OrganizationCtrl.list()).flashing("error" -> Messages("error.loading.organization"))
      }
  }

  /**
   * Display the details of a [[Person]]al [[Homepage]].
   *
   * @param pid Identifier of the [[Person]] involved.
   * @return
   */
  def showPersonHomepage(pid: Long) = isAuthenticated { username =>
    implicit request =>
      val personV = Person.load(pid)
      if (personV.isSuccess) {
        val personOp = personV.toOption.get
        personOp match {
          case Some(person) => Ok(views.html.homepage(person, Homepage.getPersonHomepages(person).toOption.get))
          case None => Logger.error(s"Failed to load person with ID '$pid'. Does not exist")
            Redirect(routes.PersonCtrl.list()).flashing("error" -> Messages("error.loading.person"))
        }
      } else {
        Logger.error(personV.toString, personV.toEither.left.get)
        Redirect(routes.PersonCtrl.list()).flashing("error" -> Messages("error.loading.person"))
      }
  }

  /**
   * Store the form data for an [[Organization]] [[Homepage]].
   *
   * @param oid Identifier of the [[Organization]] involved.
   * @return
   */
  def submitOrgHomepage(oid: Long) = isAuthenticated { username =>
    implicit request =>
      hpOrgForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the homepage form.")
          BadRequest(views.html.homepageOrgForm(errors, oid))
        },
        hp => {
          val orgV = Organization.load(oid)
          orgV match {
            case Success(orgOp) => orgOp match {
              case Some(org) => val resultV = Homepage.saveOrgHomepage(org, hp)
                resultV match {
                  case Success(result) =>
                    Redirect(routes.HomepageCtrl.showOrgHomepage(oid)).flashing("success" -> Messages("success" +
                      ".storing.homepage"))
                  case Failure(t) => Logger.error(resultV.toString, t)
                    Redirect(routes.OrganizationCtrl.show(oid)).flashing("error"-> Messages("error.storing" +
                      ".organization.homepage"))
                }
              case None => Logger.error(s"Failed to load organization with ID '$oid'. Does not exist.")
                Redirect(routes.OrganizationCtrl.list()).flashing("error"-> Messages("error.loading.organization"))
            }
            case Failure(t) => Logger.error(orgV.toString, t)
              Redirect(routes.OrganizationCtrl.show(oid)).flashing("error"-> Messages("error.storing" +
                ".organization.homepage"))
          }
        })
  }

  /**
   * Store the form data of a [[Person]]al [[Homepage]].
   *
   * @param pid Identifier of the [[Person]] involved.
   * @return
   */
  def submitPersonHomepage(pid: Long) = isAuthenticated { username =>
    implicit request =>
      hpPersForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the email form.")
          BadRequest(views.html.homepageForm(errors, pid))
        },
        hp => {
          val personV = Person.load(pid)
          if (personV.isSuccess) {
            val personOp = personV.toOption.get
            personOp match {
              case Some(person) => Logger.debug(s"Storing homepage '${hp._1.url}' for person '${person.fullname}")
                val result = Homepage.savePersonHomepage(person, hp._1, hp._2)
                if (result.isSuccess) {
                  Redirect(routes.HomepageCtrl.showPersonHomepage(pid)).flashing("success" -> Messages("success.succeededToStoreHomepage"))
                } else {
                  Logger.error(result.toString, result.toEither.left.get)
                  BadRequest(views.html.homepageForm(hpPersForm, pid)).flashing("error" -> Messages("error.failedToStoreHomepage"))
                }
              case None => Logger.error(s"Failed to load person with ID '$pid'. Does not exist.")
                Redirect(routes.PersonCtrl.list()).flashing("error" -> Messages("error.loading.person"))
            }
          } else {
            Logger.error(personV.toString, personV.toEither.left.get)
            Redirect(routes.PersonCtrl.list()).flashing("error" -> Messages("error.loading.person"))
          }
        })
  }
}