/**
 *
 */
package util

/**
 * Categorization for documents.
 * 
 * @author andreas
 * @version 0.0.1, 2015-03-08
 */
sealed abstract class DocumentType(val id: Int, val name: String) {

  /**
   * Get a [[util.DocumentType]] by its identifier.
   *
   * @param id The identifier of the [[util.DocumentType]] sought for.
   * @return The [[util.DocumentType]] instance if one with that <em>id</em> exists.
   */
  def getDocumentType(id: Int): Option[DocumentType] = {

    if (Option(id).isDefined) {
      id match {
        case 1 => Some(Protocol)
        case 2 => Some(Article)
        case 3 => Some(Agenda)
        case 4 => Some(Circular)
        case 5 => Some(Charter)
        case 6 => Some(Form)
        case 7 => Some(Database)
        case 8 => Some(Miscellaneous)
        case _ => None
      }
    } else None
  }

  def getDocumentType(docType: String): Option[DocumentType] = {
    if (Option(docType).isDefined) {
      docType match {
        case "Protocol" => Some(Protocol)
        case "Article" => Some(Article)
        case "Agenda" => Some(Agenda)
        case "Circular" => Some(Circular)
        case "Charter" => Some(Charter)
        case "Form" => Some(Form)
        case "Database" => Some(Database)
        case "Miscellaneous" => Some(Miscellaneous)
        case _ => None
      }
    } else None
  }

  /**
   * Retrieve all existing [[util.DocumentType]]s.
   */
  def getDocumentTypes(): Seq[DocumentType] = {
    Seq(Protocol, Article, Agenda, Circular, Charter, Form, Database, Miscellaneous)
  }
  
  /**
   * Redefine the hashing function.
   *
   * @return The hash of this object.
   */
  override def hashCode(): Int = this.id

  /**
   * {@inheritDoc}
   */
  override def toString(): String = {
    name
  }
  
   /**
   * Redefine the inherited method to limit equality to instances of the same type.
   *
   * @param other The instance to compare this instance with.
   * @return <code>true</code> if the other instance is of the same type, <code>false</code> otherwise.
   */
  def canEqual(other: Any) = other.isInstanceOf[DocumentType]

  /**
   * Redefine the comparison function.
   *
   * @param other The instance to compare to this instance.
   * @return <code>true</code> if the instance are the same or equal, <code>false</code> otherwise.
   */
  override def equals(other: Any): Boolean = other match {
    case that: DocumentType =>
      if (this eq that) true
      else {
        (that.## == this.##) &&
          (that canEqual this) &&
          (this.id == that.id)
      }
    case _ => false
  }
}

case object Protocol extends DocumentType(1, "Protocol")
case object Article extends DocumentType(2, "Article")
case object Agenda extends DocumentType(3, "Agenda")
case object Circular extends DocumentType(4, "Circular")
case object Charter extends DocumentType(5, "Charter")
case object Form extends DocumentType(6, "Form")
case object Database extends DocumentType(7, "Database")
case object Miscellaneous extends DocumentType(8, "Miscellaneous")