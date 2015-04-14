package controllers

import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages
import play.api.Logger
import play.api.mvc._
import play.api.Play.current
import java.io.{FileNotFoundException, File}

import controllers.ext.{ProvidesCtx, Security, SublistRetrieverAndAdder}
import models.{Documents, Document}
import models.to.DocumentUploadTO
import util.{Article, CustomFormatters, DocumentType}

/**
 * Controller for document handling.
 *
 * @author andreas
 * @version 0.0.2, 2015-04-03
 */
object DocumentCtrl extends Controller with ProvidesCtx with Security {

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
   * Delete the document identified by the goiven <em>id</em>.
   *
   * Note, that both the database entry as well as the physical file are deleted.
   *
   * @param id The identifier of the document to delete.
   * @return
   */
  def delete(id: Long) = isAuthorized("delete.document") { username =>
  implicit request =>
    require(Option(id).isDefined)
    val redirect = Redirect(routes.DocumentCtrl.overview())
    val doc = Documents.get(id)
    if (doc.isDefined) {
      this.deleteFile(doc.get.url)
      val count = Documents.delete(id)
      if (count > 0) {
        redirect.flashing("success" -> Messages("success.succeeded.to.delete.document"))
      } else {
        redirect.flashing("error" -> Messages("error.failed.to.delete.document"))
      }
    } else {
      redirect.flashing("error" -> Messages("error.document.does.not.exist"))
    }


  }

  /**
   * Send the file that is hidden behind the given document <em>id</em>
   * @param id The document ID to identify the document and, thus, the file.
   * @return
   */
  def download(id: Long) = isAuthorized("download.document") { username =>
    implicit request =>
    require(Option(id).isDefined)
    val doc = Documents.get(id)
    if (doc.isDefined) {
      Ok.sendFile(new File(doc.get.url))
    } else {
      Logger.error(s"User '$username' tried to download non-existing document with ID '$id'")
      NotFound
    }
  }

  /**
   * Display the entry page of the document section with the 10 latest documents.
   *
   * @return
   */
  def overview(limit: Int = 10) = isAuthorized("view.documents") {  username =>
    implicit request =>
    val documents = Documents.getLatest(10)
    if (documents.isSuccess) {
      Ok(views.html.documents(documents.toOption.get, None))
    } else {
      Logger.error("Failed to load latest documents.")
      Ok(views.html.documents(List(), None))
    }
  }

  /**
   * Display the <em>limit</em> latest documents of the specified <em>category</em>.
   *
   * @param category The category to restrict the documents to.
   * @param limit The maximum number of documents to list.
   * @return
   */
  def listByCategory(category: String, limit: Int = 10) = isAuthorized("view.documents") { username =>
    implicit request =>
      val cat = Article.getDocumentType(category)
      if (cat.isDefined) {
        val documents = Documents.getAllByCategory(cat.get)
        if (documents.isSuccess) {
          Ok(views.html.documents(documents.toOption.get, cat))
        } else {
          Logger.error(s"Failed to load documents of category $category.")
          Ok(views.html.documents(List(), cat))
        }
      } else {
        Logger.error(s"Category '$category' is unknwon.")
        BadRequest
      }
  }

  /**
   * Display the document identified by <em>id</em>.
   *
   * @param id Identifier of the document to be displayed.
   * @return
   */
  def show(id: Long) = isAuthorized("view.documents") { username =>
    implicit request =>
    val doc = Documents.get(id)
    if (doc.isDefined) {
      Ok(views.html.document(doc.get))
    } else {
      Logger.error(s"Document with ID '$id' could not be found.")
      NotFound
    }
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
        val path = getPath(docTO.category.toString())
        val filename = path + f.filename
        Logger.info(s"Moving uploaded file '${f.filename}' to '$path'")

        try {
          val file = new File(filename)
          f.ref.moveTo(file, replace = true)

          val doc = Document(None, docTO.title, docTO.description, docTO.category, filename, None, Some(file.length()), creator="")
          Documents.saveOrUpdate(doc)
          Redirect(routes.DocumentCtrl.overview()).flashing("success" -> Messages("document.upload.successful"))
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

  /**
   * Create the path for a document to be stored.
   *
   * <p>
   *   The base part of the path is defined in the configuration value <em>file.upload.folder</em>.
   * </p>
   *
   * @param category The category is the last part of the path.
   * @return The full path string where to store the document.
   */
  private def getPath(category: String): String = {
    var path = current.configuration.getString("file.upload.folder").get
    if (!path.endsWith("/")) {
      path += "/";
    }
    path += category + "/"
    path
  }

  /**
   * Delete the file given by the <em>url</em>
   * @param url The file name, at best fully qualified.
   * @return <code>true</code> if the file was successfully removed, <code>false</code> otherwise.
   */
  private def deleteFile(url: String): Boolean = {
    val f = new File(url)
    if (f.exists()) {
      f.delete()
    } else {
      Logger.error(s"File name '$url' does not exist so it could not be deleted.")
      false
    }
  }
}