/**
 *
 */
package util

/**
 * @author andreas
 * @version 0.0.1, 2013-03-19
 */
sealed abstract class Privacy(val id : Int, val name: String) {
  
  /**
   * Retrieve the {@link PhoneType} that matches the specified identifier.
   * 
   * @param id The identifier of the {@link Privacy} instance.
   * @return A {@link Privacy} wrapped in a {@link Some} instance if an instance with the given identifier was found
   *  {@link None} otherwise.
   */
  def getPrivacy(id: Int): Option[Privacy] = {
    
    if (Option(id).isDefined) {
      id match {
        case 1 => Option(Private)
        case 2 => Option(MembersPrivate)
        case 3 => Option(MembersPublic)
        case 4 => Option(Public)
        case _ => None
      }
    } else None
  }
  
  def getPrivacyLevels(): Seq[Privacy] = {
    Seq(Private, MembersPrivate, MembersPublic, Public)
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
 * Indicates the most strict privacy level for private viewing only.
 */
case object Private extends Privacy(1, "private")

/**
 * Indicates the second most restricted privacy level to be viewed by club members only.
 */
case object MembersPrivate extends Privacy(2, "membersprivate")

/**
 * Indicates the third most restricted privacy level to be viewed by umbrella organizations members only.
 */
case object MembersPublic extends Privacy(3, "memberspublic")

/**
 * Indicates the least restrictive level to be viewd publicly.
 */
case object Public extends Privacy(4, "public")
