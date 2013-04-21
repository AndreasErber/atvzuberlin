/**
 *
 */
package util

/**
 * @author andreas
 * @version 0.0.1, 2013-04-20
 */
sealed abstract class FormOfAddress(val id : Int, val name: String) {

  def getFormOfAddress(id: Int): Option[FormOfAddress] = {
    
    if (Option(id).isDefined) {
      id match {
        case 1 => Option(AnHerrn)
        case 2 => Option(AnFrau)
        case 3 => Option(AnHerrnUndFrau)
        case 4 => Option(Mr)
        case 5 => Option(Mrs)
        case 6 => Option(Sr)
        case 7 => Option(Sra)
        case 8 => Option(Esv)
        case 9 => Option(Esvn)
        case 10 => Option(Ew)
        case 11 => Option(AnDie)
        case 12 => Option(AnDen)
        case 13 => Option(AnDieHerren)
        case _ => None
      }
    } else None
  }
  
  def getFormsOfAddresses(): Seq[FormOfAddress] = {
    Seq(AnHerrn, AnFrau, AnHerrnUndFrau, Mr, Mrs, Sr, Sra, Esv, Esvn, Ew, AnDie, AnDen, AnDieHerren)
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

case object AnHerrn extends FormOfAddress(1, "An Herrn");
case object AnFrau extends FormOfAddress(2, "An Frau");
case object AnHerrnUndFrau extends FormOfAddress(3, "An Herrn und Frau");
case object Mr extends FormOfAddress(4, "Mr");
case object Mrs extends FormOfAddress(5, "Mrs");
case object Sr extends FormOfAddress(6, "Sr");
case object Sra extends FormOfAddress(7, "Sra");
case object Esv extends FormOfAddress(8, "An eine sehr verehrliche");
case object Esvn extends FormOfAddress(9, "An einen sehr verehrlichen");
case object Ew extends FormOfAddress(10, "An ein wohllöbliches");
case object AnDie extends FormOfAddress(11, "An die");
case object AnDen extends FormOfAddress(12, "An den");
case object AnDieHerren extends FormOfAddress(13, "An die Herren");
