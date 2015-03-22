package controllers

import controllers.ext.ProvidesCtx
import controllers.ext.Security
import controllers.ext.SublistRetrieverAndAdder
import models.{ Person, Persons }
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages
import play.api.Logger
import play.api.mvc._
import play.api.mvc.Security._
import play.api.Play.current
import util.{ CustomFormatters, DocumentType }
import models.to.DocumentUploadTO
import java.io.File

object DocumentCtrl extends Controller with ProvidesCtx with Security with SublistRetrieverAndAdder {

  implicit val charFormatter = CustomFormatters.documentTypeFormatter

  val documentTypeMapping = of[DocumentType]

  //  val documentUploadForm: Form[DocumentUploadTO] = Form(
  //		  mapping("title" -> nonEmptyText,
  //		      "description" -> optional(text),
  //		      "category" -> documentTypeMapping,
  //		      "file" -> new File)
  //  )

  def upload = Action(parse.temporaryFile) { request =>
    request.body.moveTo(new File("/tmp/uploaded"), false)
    Ok("File successfully uploaded.")
  }
}