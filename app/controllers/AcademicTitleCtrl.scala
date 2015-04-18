/**
 *
 */
package controllers

import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages
import play.api.Logger
import play.api.mvc._

import controllers.ext.ProvidesCtx
import controllers.ext.Security
import models.{AcademicTitle, PersonHasTitle, Person}
import util.CustomFormatters

/**
 * Controller to handle requests on [[AcademicTitle]]s.
 *
 * @author andreas
 * @version 0.0.8, 2015-04-18
 */
object AcademicTitleCtrl extends Controller with ProvidesCtx with Security {

  implicit val personFormatter = CustomFormatters.personFormatter
  val personMapping = of[Person]

  implicit val titleFormatter = CustomFormatters.academicTitleFormatter
  val titleMapping = of[AcademicTitle]

  /**
   * Form to handle [[AcademicTitle]]s.
   */
  val titleForm = Form[AcademicTitle] {
    mapping(
      "id" -> optional(longNumber),
      "abbr" -> nonEmptyText,
      "maleForm" -> optional(text),
      "femaleForm" -> optional(text),
      "isPrefix" -> boolean,
      "created" -> longNumber,
      "creator" -> text,
      "modified" -> optional(longNumber),
      "modifier" -> optional(text))(AcademicTitle.apply)(AcademicTitle.unapply)
  }

  /**
   * Form to handle relations between [[AcademicTitle]]s and [[Person]]s.
   */
  val titlePersonForm = Form[AcademicTitle] {
    "atid" -> titleMapping
  }

  /**
   * Display a form to add an [[AcademicTitle]] to a [[Person]] identified by <em>pid</em>
   *
   * @param pid Identifier of the person to provide the form to select a title to add.
   * @return A response with an HTTP status code 200 an a title person form for a payload.
   */
  def addPersonTitle(pid: Long) = isAuthenticated { username =>
    implicit request =>
      val titles = AcademicTitle.getAll
      if (titles.isSuccess) {
        Ok(views.html.academicTitlePersonForm(titlePersonForm, titles.toOption.get, pid))
      } else {
        Logger.debug("Failed to load list of titles.", titles.toEither.left.get)
        Ok(views.html.academicTitlePersonForm(titlePersonForm, Nil, pid))
      }
  }

  /**
   * Display a form to create a new [[AcademicTitle]].
   *
   * @return A response with HTTP status code 200 and an empty form.
   */
  def create = isAuthenticated { username =>
    implicit request =>
      Ok(views.html.academicTitleForm(titleForm))
  }

  /**
   * Remove the [[AcademicTitle]] identified by <em>id</em>.
   *
   * @param id Identifier of the [[AcademicTitle]] to remove.
   * @return A redirect to the list of [[AcademicTitle]]s flashing either success or error.
   */
  def delete(id: Long) = isAuthenticated { username =>
    implicit request =>
      val result = AcademicTitle.delete(id)
      if (result.isSuccess) {
        Logger.debug(s"Successfully deleted academic title with ID $id.")
        Redirect(routes.AcademicTitleCtrl.list()).flashing("success" -> Messages("success.deleting.academic.title"))
      } else {
        Logger.error(result.toString, result.toEither.left.get)
        Redirect(routes.AcademicTitleCtrl.list()).flashing("error" -> Messages("error.deleting.academic.title"))
      }
  }

  /**
   * Remove the relation between an [[AcademicTitle]] and a [[Person]]
   *
   * @param pid Identifier of the [[Person]] involved.
   * @param id Identifier of the [[AcademicTitle]] involved.
   */
  def deletePersonTitle(pid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>
      Logger.info(s"The removal of the association of title with ID '$id' to person with ID '$pid' was requested.")
      val result = PersonHasTitle.delete(pid, id)
      if (result.isSuccess) {
        Logger.debug(s"Successfully deleted association from academic title '$id' to person '$pid'.")
        Redirect(routes.PersonCtrl.show(pid)).flashing("success" -> Messages("success.deleting.person.academicTitle.relation"))
      } else {
        Logger.error(result.toString, result.toEither.left.get)
        Redirect(routes.PersonCtrl.show(pid)).flashing("error" -> Messages("error.deleting.person.academicTitle.relation", pid))
      }
  }

  /**
   * Display a form to modify an existing [[AcademicTitle]] instance.
   *
   * @param id Identifier of the [[AcademicTitle]] to load into the form.
   * @return A response with HTTP status code 200 and the [[AcademicTitle]] for a payload. In case of error
   *         a redirect to the list [[AcademicTitle]]s is returned.
   */
  def edit(id: Long) = isAuthenticated { username =>
    implicit request =>
      val acadTitleV = AcademicTitle.load(id)
      if (acadTitleV.isSuccess) {
        val acadTitleOp = acadTitleV.toEither.right.get
        acadTitleOp match {
          case None =>
            Logger.debug(s"Cannot find academic title with ID $id.")
            Redirect(routes.AcademicTitleCtrl.list()).flashing("error" -> Messages("error.loading.academic.title"))
          case Some(ev) =>
            Logger.logger.debug(s"Preparing editing of academic title with ID $id.")
            Ok(views.html.academicTitleForm(titleForm.fill(ev)))
          case _ => NotFound
        }
      } else {
        Logger.error(s"Failed to load academic titles.")
        Redirect(routes.AcademicTitleCtrl.list()).flashing("error" -> Messages("error.loading.academic.titles"))
      }
  }

  /**
   * Display a form to modify the relation between a [[Person]] and an [[AcademicTitle]].
   *
   * @param pid Identifier of the [[Person]] involved
   * @param id Identifier of the current [[AcademicTitle]] involved
   * @return A response with HTTP status code 200 or in case of error a redirect to display the
   *         [[Person]] involved flashing an error message.
   */
  def editPersonTitle(pid: Long, id: Long) = isAuthenticated { username =>
    implicit request =>
      val acadTitleV = AcademicTitle.load(id)
      if (acadTitleV.isSuccess) {
        val acadTitleOp = acadTitleV.toEither.right.get
        if (acadTitleOp.isDefined) {
          val titles = AcademicTitle.getAll
          if (titles.isSuccess) {
            Ok(views.html.academicTitlePersonForm(titlePersonForm.fill(acadTitleOp.get), titles.toOption.get,
              pid))
          } else {
            Redirect(routes.PersonCtrl.show(pid)).flashing("error" -> Messages("error.loading.academic.titles"))
          }
        } else {
          Logger.error(s"Failed to load academic title with ID '$id' because it does not exist.")
          Redirect(routes.PersonCtrl.show(pid)).flashing("error" -> Messages("error.loading.academic.title"))
        }
      } else {
        Logger.error(s"Failed to load academic title with ID '$id': "+ acadTitleV.toEither.left.get.getMessage)
        Redirect(routes.PersonCtrl.show(pid)).flashing("error" -> Messages("error.loading.academic.title"))
      }
  }

  /**
   * Display the list of all existing [[AcademicTitle]].
   *
   * @return A response with HTTP status code 200 and the list of [[AcademicTitle]]s for a payload. In case
   *         of error, a response with status code 400.
   */
  def list = Action { implicit request =>
    val result = AcademicTitle.getAll
    if (result.isSuccess) {
      Ok(views.html.academicTitleList(result.toOption.get.sortBy(at => at.id)))
    } else {
      Logger.error(result.toString, result.toEither.left.get)
      BadRequest(Messages("error.loading.academic.titles"))
    }
  }

  /**
   * Display the details of the [[AcademicTitle]] identified by <em>id</em>
   *
   * @param id Identifier of the [[AcademicTitle]] to be display in detail.
   * @return  An HTTP response with status code 200 and the details of the [[AcademicTitle]] found by the
   *          <em>id</em>. In case of error a redirect to the list of [[AcademicTitle]]s is returned.
   */
  def show(id: Long) = Action { implicit request =>
    val acadTitleV = AcademicTitle.load(id)
    if (acadTitleV.isSuccess) {
      val acadTitleOp = acadTitleV.toEither.right.get
      acadTitleOp match {
        case None =>
          Logger.debug(s"No academic title with ID $id found.")
          Redirect(routes.AcademicTitleCtrl.list()).flashing("error" -> Messages("error.loading.academic.title"))
        case Some(at) =>
          Logger.logger.debug(s"Found academic title with ID $id.")
          Ok(views.html.academicTitle(at))
        case _ => NotFound
      }
    } else {
      Logger.error(s"Failed to load academic title with ID '$id': " + acadTitleV.toEither.left.get.getMessage)
      Redirect(routes.AcademicTitleCtrl.list()).flashing("error" -> Messages("error.loading.academic.title"))
    }
  }

  /**
   * Handle the form data that was entered by the user to create a new or modify an existing
   * [[AcademicTitle]].
   *
   * @return A redirect to display the details of the newly created [[AcademicTitle]], in case of error a
   *         [[BadRequest]] displaying the error.
   */
  def submit = isAuthenticated { username =>
    implicit request =>
      titleForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the academic title form.")
          BadRequest(views.html.academicTitleForm(errors))
        },
        at => {
          Logger.debug(s"Storing academic title $at.")
          val result = AcademicTitle.saveOrUpdate(at)
          if (result.isSuccess) {
            Redirect(routes.AcademicTitleCtrl.show(result.toOption.get.id.get)).flashing("success" -> Messages("success.storing.academic.title"))
          } else {
            Logger.error(result.toString, result.toEither.left.get)
            BadRequest(views.html.academicTitleForm(titleForm)).flashing("error" -> Messages("error.storing.academic.title"))
          }
        })
  }

  /**
   * Handle the form data that was submitted to create a new or modify an existing relation between
   * a [[Person]] and an [[AcademicTitle]].
   *
   * @param pid Identifier of the [[Person]] involved.
   * @return A redirect to the display of the [[Person]] details. In case the person does not exist
   *         the redirect leads to the list of [[Person]]s to be displayed. In case of form errors
   *         a [[BadRequest]] is issued.
   */
  def submitPersonTitle(pid: Long) = isAuthenticated { username =>
    implicit request =>
      titlePersonForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the academic title person form.")
          val result = AcademicTitle.getAll
          if (result.isSuccess) {
            BadRequest(views.html.academicTitlePersonForm(errors, result.toOption.get, pid))
          } else {
            Logger.error(result.toString, result.toEither.left.get)
            BadRequest(views.html.academicTitlePersonForm(errors, Nil, pid))
          }
        },
        at => {
          val p = Person.load(pid)
          if (!p.isDefined) {
            Logger.error(s"Failed to load person with ID $pid.")
            Redirect(routes.PersonCtrl.list()).flashing("error" -> Messages("error.loading.person", pid))
          } else {
            Logger.debug("Storing academic title with ID $at.id for person $p.get.name.")
            val result = PersonHasTitle.add(p.get, at)
            // display the person in detail
            if (result.isSuccess) {
              Redirect(routes.PersonCtrl.show(p.get.id.get)).flashing("success" -> Messages("success.storing.academic.title"))
            } else {
              Logger.error(result.toString, result.toEither.left.get)
              Redirect(routes.PersonCtrl.show(p.get.id.get)).flashing("error" -> Messages("error.storing.academic.title"))
            }
          }
        })
  }
}