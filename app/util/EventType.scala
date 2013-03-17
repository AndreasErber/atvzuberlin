/**
 *
 */
package util

/**
 * @author andreas
 *
 */
sealed abstract class EventType(val id: Int, val name: String) {

  def getEventType(id: Int): Option[EventType] = {
    
    if (Option(id).isDefined) {
      id match {
        case 1 => Option(Academic)
        case 2 => Option(Sport)
        case 3 => Option(Atv)
        case 4 => Option(Atb)
        case 5 => Option(Cultural)
        case 6 => Option(Handball)
        case 7 => Option(Actives)
        case 8 => Option(Burschen)
        case _ => None
      }
    } else None
  }
  
  def getEventTypes(): Seq[EventType] = {
    Seq(Academic, Sport, Atv, Atb, Cultural, Handball, Actives, Burschen)
  }
  
  /**
   * Redefine the hashing function.
   *
   * @return The hash of this object.
   */
  override def hashCode(): Int = this.id
  
  override def toString() : String = {
    name 
  }
}

case object Academic extends EventType(1, "Academic")
case object Sport extends EventType(2, "Sport")
case object Atv extends EventType(3, "ATV")
case object Atb extends EventType(4, "ATB")
case object Cultural extends EventType(5, "Cultural")
case object Handball extends EventType(6, "Handball")
case object Actives extends EventType(7, "Actives")
case object Burschen extends EventType(8, "Burschen")
