/**
 *
 */
package models.to

import util.DocumentType
import java.io.File

/**
 * Transfer object for document upload data
 *
 * @author andreas
 * @version 0.0.1, 2015-03-08
 */
case class DocumentUploadTO(
  val title: String,
  val description: Option[String],
  val category: DocumentType,
  val file: File) {

  require(Option(title).isDefined)
  require(Option(category).isDefined)
  require(Option(file).isDefined)
}