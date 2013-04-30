package util

import play.api.data.format.Formatter
import play.api.data.FormError
import play.api.Logger
import scala.util.control.Exception
import java.text.SimpleDateFormat
import java.util.Date
import java.util.Calendar
import models.Person
import models.User
import models.Email
import java.sql.Timestamp
import models.Country
import java.text.ParseException
import play.api.i18n.Messages
import models.AcademicTitle
import models.Event

/**
 * @author andreas
 * @version 0.1.4, 2013-04-28
 */
object CustomFormatters {

  /**
   * Maps an {@link Event} instance to its identifier and vice versa. The identifier is taken and returned as String.
   */
  val eventFormatter = new Formatter[Event] {

    def bind(key: String, data: Map[String, String]) = {
      data.get(key).toRight {
        Seq(FormError(key, Messages("error.required"), Nil))
      }.right.flatMap { id =>
        Exception.allCatch[Event].either(Event.load(id.toLong).get).left.map {
          e => Seq(FormError(key, Messages("error.failedToLoadEvent", id), Nil))
        }
      }
    }

    def unbind(key: String, e: Event) = Map(key -> e.id.get.toString())
  }

  /**
   * Maps a {@link LetterSalutation} instance to its identifier and vice versa. The identifier is taken and returned as String.
   */
  val letterSalutationFormatter = new Formatter[LetterSalutation] {

    def bind(key: String, data: Map[String, String]) = {
      data.get(key).toRight {
        Seq(FormError(key, Messages("error.required"), Nil))
      }.right.flatMap { id =>
        Exception.allCatch[LetterSalutation].either(Bbr.getLetterSalutation(id.toInt).get).left.map {
          e => Seq(FormError(key, Messages("error.failedToLoadLetterSalutation", id), Nil))
        }
      }
    }

    def unbind(key: String, ls: LetterSalutation) = Map(key -> ls.id.toString())
  }
  
  /**
   * Maps a {@link FormOfAddress} instance to its identifier and vice versa. The identifier is taken and returned as String.
   */
  val formOfAddressFormatter = new Formatter[FormOfAddress] {

    def bind(key: String, data: Map[String, String]) = {
      data.get(key).toRight {
        Seq(FormError(key, Messages("error.required"), Nil))
      }.right.flatMap { id =>
        Exception.allCatch[FormOfAddress].either(Mr.getFormOfAddress(id.toInt).get).left.map {
          e => Seq(FormError(key, Messages("error.failedToLoadFormOfAddress", id), Nil))
        }
      }
    }

    def unbind(key: String, foa: FormOfAddress) = Map(key -> foa.id.toString())
  }
  
  
  /**
   * Maps an {@link AcademicTitle} instance to its identifier and vice versa. The identifier is taken and returned as String.
   */
  val academicTitleFormatter = new Formatter[AcademicTitle] {

    def bind(key: String, data: Map[String, String]) = {
      data.get(key).toRight {
        Seq(FormError(key, Messages("error.required"), Nil))
      }.right.flatMap { id =>
        Exception.allCatch[AcademicTitle].either(AcademicTitle.load(id.toInt).get).left.map {
          e => Seq(FormError(key, Messages("error.failedToLoadAcademicTitle", id), Nil))
        }
      }
    }

    def unbind(key: String, at: AcademicTitle) = Map(key -> at.id.toString())
  }
  
  /**
   * Maps a user instance to its username and vice versa.
   */
  val userFormatter = new Formatter[User] {

    def bind(key: String, data: Map[String, String]) = {
      data.get(key).toRight {
        Seq(FormError(key, Messages("error.required"), Nil))
      }.right.flatMap { username =>
        Exception.allCatch[User].either(User.findByName(username).toOption.get).left.map {
          e => Seq(FormError(key, Messages("error.failedToFindUserByName", username), Nil))
        }
      }
    }

    def unbind(key: String, u: User) = Map(key -> u.username)
  }

  /**
   * Maps a UserRole instance to its identifier and vice versa. The identifier is taken and returned as String.
   */
  val userRoleFormatter = new Formatter[UserRole] {

    def bind(key: String, data: Map[String, String]) = {
      data.get(key).toRight {
        Seq(FormError(key, Messages("error.required"), Nil))
      }.right.flatMap { id =>
        Exception.allCatch[UserRole].either(StandardUser.getUserRole(id.toInt).get).left.map {
          e => Seq(FormError(key, Messages("error.failedToLoadUserRole", id), Nil))
        }
      }
    }

    def unbind(key: String, u: UserRole) = Map(key -> u.id.toString())
  }

  /**
   * Maps a UsageType instance to its identifier and vice versa. The identifier is taken and returned as String.
   */
  val usageTypeFormatter = new Formatter[UsageType] {

    def bind(key: String, data: Map[String, String]) = {
      data.get(key).toRight {
        Seq(FormError(key, Messages("error.required"), Nil))
      }.right.flatMap { id =>
        Exception.allCatch[UsageType].either(Personal.getUsageType(id.toInt).get).left.map {
          e => Seq(FormError(key, Messages("error.failedToLoadUsagetype", id), Nil))
        }
      }
    }

    def unbind(key: String, u: UsageType) = Map(key -> u.id.toString())
  }

  val eventTypeFormatter = new Formatter[EventType] {

    def bind(key: String, data: Map[String, String]) = {
      data.get(key).toRight {
        Seq(FormError(key, Messages("error.required"), Nil))
      }.right.flatMap { id =>
        Exception.allCatch[EventType].either(Atv.getEventType(id.toInt).get).left.map {
          e => Seq(FormError(key, Messages("error.failedToLoadEventtype", id), Nil))
        }
      }
    }

    def unbind(key: String, et: EventType) = Map(key -> et.id.toString())
  }

  /**
   * Maps a Person instance to its identifier and vice versa. The identifier is taken and returned as String.
   */
  val personFormatter = new Formatter[Person] {

    def bind(key: String, data: Map[String, String]) = {
      data.get(key).toRight {
        Seq(FormError(key, Messages("error.required"), Nil))
      }.right.flatMap { id =>
        Exception.allCatch[Person].either(Person.load(id.toLong).get).left.map {
          e => Seq(FormError(key, Messages("error.failedToLoadPerson", id), Nil))
        }
      }
    }

    def unbind(key: String, p: Person) = Map(key -> p.id.get.toString())
  }

  /**
   * Maps an Email instance to its address string and vice versa.
   */
  val emailFormatter = new Formatter[Email] {
    def bind(key: String, data: Map[String, String]) = {
      data.get(key).toRight {
        Seq(FormError(key, Messages("error.required"), Nil))
      }.right.flatMap { string =>
        Exception.allCatch[Email].either(Email.findByAddress(string).toOption.get.get).left.map {
          e => Seq(FormError(key, Messages("error.failedToFindEmailAddress", string), Nil))
        }
      }
    }

    def unbind(key: String, e: Email) = Map(key -> e.address)
  }

  val countryFormatter = new Formatter[Country] {
    def bind(key: String, data: Map[String, String]) = {
      data.get(key).toRight {
        Seq(FormError(key, Messages("error.required"), Nil))
      }.right.flatMap { string =>
        Exception.allCatch[Country].either(Country.load(string.toInt).get).left.map {
          e => Seq(FormError(key, Messages("error.failedToLoadCountry", string), Nil))
        }
      }
    }

    def unbind(key: String, c: Country) = Map(key -> c.id.get.toString())
  }
  
  val privacyFormatter = new Formatter[Privacy] {
    def bind(key: String, data: Map[String, String]) = {
      data.get(key).toRight {
        Seq(FormError(key, Messages("error.required"), Nil))
      }.right.flatMap { id =>
        Exception.allCatch[Privacy].either(MembersPrivate.getPrivacy(id.toInt).get).left.map {
          e => Seq(FormError(key, Messages("error.failedToLoadPrivacy", id), Nil))
        }
      }
    }

    def unbind(key: String, p: Privacy) = Map(key -> p.id.toString())
  }

  val phoneFormatter = new Formatter[PhoneType] {
    def bind(key: String, data: Map[String, String]) = {
      data.get(key).toRight {
        Seq(FormError(key, Messages("error.required"), Nil))
      }.right.flatMap { id =>
        Exception.allCatch[PhoneType].either(Landline.getPhoneType(id.toInt).get).left.map {
          e => Seq(FormError(key, Messages("error.failedToLoadPhoneType", id), Nil))
        }
      }
    }

    def unbind(key: String, pt: PhoneType) = Map(key -> pt.id.toString())
  }

  /**
   * Takes the first character of the input string and returns it as a char.
   */
  val charFormatter = new Formatter[Char] {
    def bind(key: String, data: Map[String, String]) = {
      data.get(key).toRight {
        Seq(FormError(key, Messages("error.required"), Nil))
      }.right.flatMap { char =>
        Exception.allCatch[Char].either(char.charAt(0)).left.map {
          e => Seq(FormError(key, Messages("error.failedToIdentifyCharacter"), Nil))
        }
      }
    }

    def unbind(key: String, c: Char) = Map(key -> c.toString())
  }

  val sqlDateFormatter = new Formatter[java.sql.Date] {

    def bind(key: String, data: Map[String, String]) = {
      data.get(key).toRight {
        Seq(FormError(key, Messages("error.required"), Nil))
      }.right.flatMap { date =>
        Exception.allCatch[java.sql.Date].either(stringToSqlDate(date)).left.map {
          e => Seq(FormError(key, e.getMessage(), Nil))
        }
      }
    }

    def unbind(key: String, date: java.sql.Date) = Map(key -> new SimpleDateFormat("yyyy-MM-dd").format(date))
  }

  val sqlTimestampFormatter = new Formatter[Timestamp] {

    def bind(key: String, data: Map[String, String]) = {
      data.get(key).toRight {
        Seq(FormError(key, Messages("error.required"), Nil))
      }.right.flatMap { timestamp =>
        Exception.allCatch[Timestamp].either(stringToSqlTimestamp(timestamp)).left.map {
          e => Seq(FormError(key, e.getMessage(), Nil))
        }
      }
    }

    def unbind(key: String, ts: Timestamp) = Map(key -> new SimpleDateFormat("yyyy-MM-dd HH:mm").format(ts))
  }

  /**
   * Maps a java.util.Date to its timestamp (long value) and vice versa. The identifier is taken and returned as String.
   */
  val dateFormatter = new Formatter[Date] {

    def bind(key: String, data: Map[String, String]) = {
      data.get(key).toRight {
        Seq(FormError(key, Messages("error.required"), Nil))
      }.right.flatMap { date =>
        Exception.allCatch[Date].either(CustomFormatters.stringToDate(date)).left.map {
          e => Seq(FormError(key, e.getMessage(), Nil))
        }
      }
    }

    def unbind(key: String, date: Date) = Map(key -> date.getTime().toString())
  }

  /**
   * Maps a java.util.Date to its timestamp (long value) and vice versa. The identifier is taken and returned as String.
   * Note, that if the input string is empty the current timestamp is used as a default.
   */
  val dateDefaultNowFormatter = new Formatter[Date] {

    def bind(key: String, data: Map[String, String]) = {
      data.get(key).toRight {
        Seq(FormError(key, Messages("error.required"), Nil))
      }.right.flatMap { date =>
        Exception.allCatch[Date].either(CustomFormatters.stringToDateDefaultNow(date)).left.map {
          e => Seq(FormError(key, e.getMessage(), Nil))
        }
      }
    }

    def unbind(key: String, date: Date) = Map(key -> date.getTime().toString())
  }

  /**
   * Helper to turn a String into a java.util.Date.
   */
  def stringToDate(s: String): Date = {

    try {
      val date = s.toLong
      val cal = Calendar.getInstance()
      cal.setTimeInMillis(date)
      cal.getTime()
    } catch {
      case e: Throwable => throw new RuntimeException(Messages("error.failedToParseDate", s))
    }
  }

  /**
   * Helper to turn a String into a java.util.Date. If the input value is empty the current timestamp is taken as default value.
   */
  def stringToDateDefaultNow(s: String): Date = {

    try {
      val date = if (s.length() <= 0) System.currentTimeMillis() else s.toLong
      val cal = Calendar.getInstance()
      cal.setTimeInMillis(date)
      cal.getTime()
    } catch {
      case e: Throwable => throw new RuntimeException(Messages("error.failedToParseDate", s))
    }
  }

  /**
   * Can parse a date string of two different formats. Both 'yyyy-MM-dd' and 'dd.MM.yyyy' are accepted.
   */
  def stringToSqlDate(s: String): java.sql.Date = {

    val us = new SimpleDateFormat("yyyy-MM-dd")
    val de = new SimpleDateFormat("dd.MM.yyyy")
    var date: Date = null
    try {
      date = us.parse(s)
    } catch {
      case e1: ParseException =>
        try {
          date = de.parse(s)
        } catch {
          case e2: ParseException => throw new RuntimeException(Messages("error.failedToParseDate", s))
        }
    }
    val cal = Calendar.getInstance()
    cal.setTime(date)
    new java.sql.Date(cal.getTimeInMillis())
  }

  /**
   * Can parse a timestamp string of two different formats. Both 'yyyy-MM-dd HH:mm' and 'dd.MM.yyyy HH:mm' are accepted.
   */
  def stringToSqlTimestamp(s: String): Timestamp = {

    val us = new SimpleDateFormat("yyyy-MM-dd HH:mm")
    val de = new SimpleDateFormat("dd.MM.yyyy HH:mm")
    var date: Date = null
    try {
      date = us.parse(s)
    } catch {
      case e1: ParseException =>
        try {
          date = de.parse(s)
        } catch {
          case e2: ParseException => throw new RuntimeException(Messages("error.failedToParseDate", s))
        }
    }
    val cal = Calendar.getInstance()
    cal.setTime(date)
    new Timestamp(cal.getTimeInMillis())
  }
}