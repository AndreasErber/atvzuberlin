/**
 *
 */
package util

/**
 * Types of events.
 * 
 * @author andreas
 * @version 0.0.2, 2015-01-07
 */
sealed abstract class EventType(val id: Int, val name: String) {

  def getEventType(id: Int): Option[EventType] = {
    
    if (Option(id).isDefined) {
      id match {
        case 1 => Option(AcademicEvent)
        case 2 => Option(SportsEvent)
        case 3 => Option(AtvEvent)
        case 4 => Option(AtbEvent)
        case 5 => Option(CulturalEvent)
        case 6 => Option(HandballEvent)
        case 7 => Option(ActivesEvent)
        case 8 => Option(BurschenEvent)
        case _ => None
      }
    } else None
  }
  
  def getEventTypes(): Seq[EventType] = {
    Seq(AcademicEvent, SportsEvent, AtvEvent, AtbEvent, CulturalEvent, HandballEvent, ActivesEvent, BurschenEvent)
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

case object AcademicEvent extends EventType(1, "Academic")
case object SportsEvent extends EventType(2, "Sport")
case object AtvEvent extends EventType(3, "ATV")
case object AtbEvent extends EventType(4, "ATB")
case object CulturalEvent extends EventType(5, "Cultural")
case object HandballEvent extends EventType(6, "Handball")
case object ActivesEvent extends EventType(7, "Actives")
case object BurschenEvent extends EventType(8, "Burschen")
