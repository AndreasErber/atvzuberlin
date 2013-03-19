/**
 *
 */
package util

/**
 * @author andreas
 * @version 0.0.1, 2013-03-19
 */
sealed abstract class PhoneType(val id : Int, val name: String) {
  
  /**
   * Retrieve the {@link PhoneType} that matches the specified identifier.
   * 
   * @param id The identifier of the {@link PhoneType} instance.
   * @return A {@link PhoneType} wrapped in a {@link Some} instance if an instance with the given identifier was found
   *  {@link None} otherwise.
   */
  def getPhoneType(id: Int): Option[PhoneType] = {
    
    if (Option(id).isDefined) {
      id match {
        case 1 => Option(Landline)
        case 2 => Option(Fax)
        case 3 => Option(Mobile)
        case _ => None
      }
    } else None
  }
  
  def getPhoneTypes(): Seq[PhoneType] = {
    Seq(Landline, Fax, Mobile)
  }
  
  override def toString() : String = {
    name
  }
  
  /**
   * Redefine the hashing function.
   *
   * @return The hash of this object.
   */
  override def hashCode(): Int = this.id
}

/**
 * Indicates a landline phone number.
 */
case object Landline extends PhoneType(1, "landline")
/**
 * Indicates a fax number.
 */
case object Fax extends PhoneType(2, "fax")

/**
 * Indicates a mobile phone number type.
 */
case object Mobile extends PhoneType(3, "mobile")
