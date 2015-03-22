/**
 *
 */
package util

/**
 * The states members of organizations might have.
 *
 * @author andreas
 * @version 0.0.3, 2015-03-08
 */
sealed abstract class MemberState(val id: Int, val name: String) {

  /**
   * Get a {@link MemberState} by its identifier.
   *
   * @param id The identifier of the {@link MemberState} sought for.
   * @return The {@link MemberState} instance if one with that <em>id</em> exists.
   */
  def getMemberState(id: Int): Option[MemberState] = {

    if (Option(id).isDefined) {
      id match {
        case 1 => Some(Hausbewohner)
        case 2 => Some(Fux)
        case 3 => Some(Aktiv)
        case 4 => Some(Inaktiv)
        case 5 => Some(Auswaertig)
        case 6 => Some(KV)
        case 7 => Some(EM)
        case 8 => Some(Ehrenhalber)
        case 9 => Some(MitturnerAk)
        case 10 => Some(MitturnerKV)
        case 11 => Some(Witwe)
        case 12 => Some(ATB)
        case 13 => Some(Korpo)
        case 14 => Some(Other)
        case _ => None
      }
    } else None
  }

  /**
   * Retrieve all existing {@link MemberState}s.
   */
  def getMemberStates(): Seq[MemberState] = {
    Seq(Hausbewohner, Fux, Aktiv, Inaktiv, Auswaertig, KV, EM, Ehrenhalber, MitturnerAk, MitturnerKV, Witwe, ATB, Korpo, Other)
  }
  
  /**
   * Retrieve the {@link MemberState}s that ATV members can have.
   */
  def getAtvMemberStates(): Seq[MemberState] = {
    Seq(Fux, Aktiv, Inaktiv, Auswaertig, MitturnerAk, KV, EM, Ehrenhalber, MitturnerKV)
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
  def canEqual(other: Any) = other.isInstanceOf[MemberState]

  /**
   * Redefine the comparison function.
   *
   * @param other The instance to compare to this instance.
   * @return <code>true</code> if the instance are the same or equal, <code>false</code> otherwise.
   */
  override def equals(other: Any): Boolean = other match {
    case that: MemberState =>
      if (this eq that) true
      else {
        (that.## == this.##) &&
          (that canEqual this) &&
          (this.id == that.id)
      }
    case _ => false
  }
}

case object Hausbewohner extends MemberState(1, "Hausbewohner")
case object Fux extends MemberState(2, "Fux")
case object Aktiv extends MemberState(3, "Aktiv")
case object Inaktiv extends MemberState(4, "Inaktiv")
case object Auswaertig extends MemberState(5, "Auswärtig")
case object KV extends MemberState(6, "Korporationsverband")
case object EM extends MemberState(7, "Ehrenmitglied")
case object Ehrenhalber extends MemberState(8, "Mitglied ehrenhalber")
case object MitturnerAk extends MemberState(9, "Mitturner Aktivitas")
case object MitturnerKV extends MemberState(10, "Mitturner Korporationsverband")
case object Witwe extends MemberState(11, "Witwe")
case object ATB extends MemberState(12, "ATB")
case object Korpo extends MemberState(13, "Korporiert")
case object Other extends MemberState(14, "-")
