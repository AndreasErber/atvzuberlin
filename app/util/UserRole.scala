/**
 *
 */
package util

/**
 * @author andreas
 * @version 0.0.2, 2013-03-03
 */
sealed abstract class UserRole(val id: Int, val name: String, val description: String) {

  def getUserRole(id: Int): Option[UserRole] = {
    if (Option(id).isDefined) {
      id match {
        case 5 => Option(StandardUser)
        case 2 => Option(Administrator)
        case 0 => Option(SuperUser)
        case _ => None
      }
    } else
      None
  }

  def getUserRoles(): Seq[UserRole] = {
    Seq(SuperUser, Administrator, StandardUser)
  }

  override def hashCode(): Int = this.id
}

case object StandardUser extends UserRole(5, "StandardUser", "A standard user to read additional data and edit own data.")
case object Administrator extends UserRole(2, "Administrator", "An administrative role to edit other users' data.")
case object SuperUser extends UserRole(0, "SuperUser", "The top administrative role.")