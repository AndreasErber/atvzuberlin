/**
 *
 */
package util

/**
 * @author andreas
 * @version 0.0.1, 2013-04-20
 */
sealed abstract class LetterSalutation(val id: Int, val name: String) {

  def getLetterSalutation(id: Int): Option[LetterSalutation] = {
    
    if (Option(id).isDefined) {
      id match {
        case 1 => Option(Lieber)
        case 2 => Option(Liebe)
        case 3 => Option(Bbr)
        case 4 => Option(Bschw)
        case 5 => Option(BbrBbr)
        case 6 => Option(Bgschw)
        case 7 => Option(Fbr)
        case 8 => Option(Fschw)
        case 9 => Option(FbrFbr)
        case 10 => Option(Fgschw)
        case 11 => Option(FschwFschw)
        case 12 => Option(SgH)
        case 13 => Option(SgF)
        case 14 => Option(SgDH)
        case 15 => Option(SportKameraden)
        case 16 => Option(Hallo)
        case 17 => Option(Gruss)
        case 18 => Option(FrauFschw)
        case 19 => Option(HerrBbr)
        case 20 => Option(FrauBschw)
        case 21 => Option(LBschwLBbr)
        case _ => None
      }
    } else None
  }
  
  def getLetterSalutations(): Seq[LetterSalutation] = {
    Seq(Lieber, Liebe, Bbr, Bschw, BbrBbr, Bgschw, Fbr, Fschw, FbrFbr, Fgschw, FschwFschw, SgH, SgF, SgDH, SportKameraden, Hallo, Gruss, FrauFschw, HerrBbr, FrauBschw, LBschwLBbr)
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

case object Lieber extends LetterSalutation(1, "Lieber")
case object Liebe extends LetterSalutation(2, "Liebe")
case object Bbr extends LetterSalutation(3, "Lieber Bundesbruder")
case object Bschw extends LetterSalutation(4, "Liebe Bundesschwester")
case object BbrBbr extends LetterSalutation(5, "Liebe Bundesbrüder")
case object Bgschw extends LetterSalutation(6, "Liebe Bundesschwestern und Bundesbrüder")
case object Fbr extends LetterSalutation(7, "Sehr geehrter Herr Farbenbruder")
case object Fschw extends LetterSalutation(8, "Sehr geehrte Farbenschwester")
case object FbrFbr extends LetterSalutation(9, "Sehr geehrte Herren Farbenbrüder")
case object Fgschw extends LetterSalutation(10, "Sehr geehrte Farbenschwestern und Farbenbrüder")
case object FschwFschw extends LetterSalutation(11, "Sehr geehrte Farbenschwestern")
case object SgH extends LetterSalutation(12, "Sehr geehrter Herr")
case object SgF extends LetterSalutation(13, "Sehr geehrte Frau")
case object SgDH extends LetterSalutation(14, "Sehr geehrte Damen und Herren")
case object SportKameraden extends LetterSalutation(15, "Liebe Sportkameraden")
case object Hallo extends LetterSalutation(16, "Hallo")
case object Gruss extends LetterSalutation(17, "Unsern Gruß zuvor!")
case object FrauFschw extends LetterSalutation(18, "Sehr geehrte Frau Farbenschwester")
case object HerrBbr extends LetterSalutation(19, "Sehr geehrter Herr Bundesbruder")
case object FrauBschw extends LetterSalutation(20, "Sehr geehrte Frau Bundesschwester")
case object LBschwLBbr extends LetterSalutation(21, "Liebe Bundesschwester, Lieber Bundesbruder")
