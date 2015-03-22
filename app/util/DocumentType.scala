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
   * Get a {@link DocumentType} by its identifier.
   *
   * @param id The identifier of the {@link DocumentType} sought for.
   * @return The {@link DocumentType} instance if one with that <em>id</em> exists.
   */
  def getDocumentType(id: Int): Option[DocumentType] = {

    if (Option(id).isDefined) {
      id match {
        case 1 => Some(Protokoll)
        case 2 => Some(Artikel)
        case 3 => Some(TO)
        case 4 => Some(Rundschreiben)
        case 5 => Some(Satzung)
        case 6 => Some(Formular)
        case 7 => Some(Datenbank)
        case 8 => Some(Sonstiges)
        case _ => None
      }
    } else None
  }

  /**
   * Retrieve all existing {@link DocumentType}s.
   */
  def getDocumentTypes(): Seq[DocumentType] = {
    Seq(Protokoll, Artikel, TO, Rundschreiben, Satzung, Formular, Datenbank, Sonstiges)
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

case object Protokoll extends DocumentType(1, "Protokoll")
case object Artikel extends DocumentType(2, "Artikel")
case object TO extends DocumentType(3, "Tagesordnung")
case object Rundschreiben extends DocumentType(4, "Rundschreiben")
case object Satzung extends DocumentType(5, "Satzung")
case object Formular extends DocumentType(6, "Formular")
case object Datenbank extends DocumentType(7, "Datenbank")
case object Sonstiges extends DocumentType(8, "Sonstiges")