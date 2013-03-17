/**
 *
 */
package util

/**
 * @author andreas
 * @version 0.0.2, 2013-03-03
 */
sealed abstract class UsageType(val id : Int, val name: String) {
  
  /**
   * Retrieve the {@link UsageType} that matches the specified identifier.
   * 
   * @param id The identifier of the {@link UsageType} instance.
   * @return A {@link UsageType} wrapped in a {@link Some} instance if an instance with the given identifier was found
   *  {@link None} otherwise.
   */
  def getUsageType(id: Int): Option[UsageType] = {
    
    if (Option(id).isDefined) {
      id match {
        case 1 => Option(Personal)
        case 2 => Option(Business)
        case 3 => Option(Honorary)
        case _ => None
      }
    } else None
  }
  
  def getUsageTypes(): Seq[UsageType] = {
    Seq(Personal, Business, Honorary)
  }
  
  /**
   * Redefine the hashing function.
   *
   * @return The hash of this object.
   */
  override def hashCode(): Int = this.id
}

/**
 * Indicates a private use.
 */
case object Personal extends UsageType(1, "Personal") {
  
  /**
   * String representation of the instance.
   */
  override def toString() : String = {
    "personal"
  }
}

/**
 * Indicates a business use.
 */
case object Business extends UsageType(2, "Business") {
  
  /**
   * String representation of the instance.
   */
  override def toString() : String = {
    "business"
  }
}

/**
 * Indicates an honorary use.
 */
case object Honorary extends UsageType(3, "Honorary") {
  
  /**
   * String representation of the instance.
   */
  override def toString() : String = {
    "honorary"
  }
}
