/**
 *
 */
package util

/**
 * The states members of organizations might have.
 *
 * @author andreas
 * @version 0.0.1, 2013-07-27
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
        case 1 => Option(Hausbewohner)
        case 2 => Option(Fux)
        case 3 => Option(Aktiv)
        case 4 => Option(Inaktiv)
        case 5 => Option(Auswaertig)
        case 6 => Option(KV)
        case 7 => Option(EM)
        case 8 => Option(Ehrenhalber)
        case 9 => Option(MitturnerAk)
        case 10 => Option(MitturnerKV)
        case 11 => Option(Witwe)
        case _ => None
      }
    } else None
  }

  /**
   * Retrieve all existing {@link MemberState}s.
   */
  def getMemberStates(): Seq[MemberState] = {
    Seq(Hausbewohner, Fux, Aktiv, Inaktiv, Auswaertig, KV, EM, Ehrenhalber, MitturnerAk, MitturnerKV, Witwe)
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

