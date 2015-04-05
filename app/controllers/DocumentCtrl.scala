package controllers

import controllers.ext.{ProvidesCtx, Security, SublistRetrieverAndAdder}
import models.{Documents, Document, Person, Persons}
import models.to.DocumentUploadTO
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages
import play.api.Logger
import play.api.mvc._
import play.api.mvc.Security._
import play.api.Play.current
import util.{CustomFormatters, DocumentType}
import java.io.{FileNotFoundException, File}

/**
 * Controller for document handling.
 *
 * @author andreas
 * @version 0.0.2, 2015-04-03
 */
object DocumentCtrl extends Controller with ProvidesCtx with Security with SublistRetrieverAndAdder {

  implicit val charFormatter = CustomFormatters.documentTypeFormatter

  val documentTypeMapping = of[DocumentType]

  val documentUploadForm: Form[DocumentUploadTO] = Form(
    mapping("title" -> nonEmptyText,
      "description" -> optional(text),
      "category" -> documentTypeMapping)(DocumentUploadTO.apply)(DocumentUploadTO.unapply)
  )

  /**
   * Display a clean document upload form.
   *
   * @return
   */
  def create = Action {
    implicit request =>
      Ok(views.html.documentUpload(documentUploadForm))
  }

  /**
   * Handle the form data and the file upload
   *
   * @return
   */
  def upload = Action(parse.multipartFormData) { implicit request =>
    // retrieve the information entered in the form
    val boundForm = documentUploadForm.bindFromRequest()
    val documentTO: Option[DocumentUploadTO] = boundForm.fold(
      errors => None,
      docTO => Some(docTO)
    )
    request.body.file("file").map { f =>
      documentTO.map { docTO =>
        var path = getPath(docTO.category.toString())
        val filename = path + f.filename
        Logger.info(s"Moving uploaded file '${f.filename}' to '$path'")

        try {
          f.ref.moveTo(new File(filename), replace = true)
          val doc = Document(None, docTO.title, docTO.description, docTO.category, filename, None, None, creator="")
          Documents.saveOrUpdate(doc)
          Ok("File successfully uploaded.")
        } catch {
          case e: FileNotFoundException => {
            Logger.error("The uploaded file could not be stored.", e)
            BadRequest(views.html.documentUpload(boundForm.withGlobalError(Messages("error.failed.to.store.uploaded.file"))))
          }
        }
      }.getOrElse {
        Logger.error("An error occurred when trying to process the the document upload form.")
        BadRequest(views.html.documentUpload(boundForm))
      }
    }.getOrElse {
      BadRequest("File not attached.")
    }
  }

  private def getPath(category: String): String = {
    var path = current.configuration.getString("file.upload.folder").get
    if (!path.endsWith("/")) {
      path += "/";
    }
    path += category + "/"
    path
  }
}