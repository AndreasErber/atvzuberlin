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
  title: String,
  description: Option[String],
  category: DocumentType) {

  require(Option(title).isDefined)
  require(Option(category).isDefined)
}