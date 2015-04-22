package controllers

import controllers.ext.ProvidesCtx
import controllers.ext.Security
import models.{AcademicTitle, Address, Email, Homepage, Person, PersonAdditionalInfo, Phone}
import util.CustomFormatters
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.Messages
import play.api.Logger
import play.api.mvc._

import scalaz.{Failure, Success}

/**
 * Controller to handle [[Person]] related requests.
 * @author andreas
 * @version 0.2.3, 2015-01-07
 */
object PersonCtrl extends Controller with ProvidesCtx with Security {

  implicit val charFormatter = CustomFormatters.charFormatter
  implicit val sqlDateFormatter = CustomFormatters.sqlDateFormatter

  val charMapping = of[Char]
  val sqlDateMapping = of[java.sql.Date]

  val personForm = Form(
    mapping(
      "id" -> optional(longNumber),
      "lastname" -> nonEmptyText,
      "firstname" -> optional(text),
      "nickname" -> optional(text),
      "birth" -> optional(sqlDateMapping),
      "death" -> optional(sqlDateMapping),
      "gender" -> charMapping,
      "created" -> longNumber,
      "creator" -> text,
      "modified" -> optional(longNumber),
      "modifier" -> optional(text))(Person.apply)(Person.unapply))

  /**
   * Display the form to create a new person.
   */
  def create = isAuthenticated { username =>
    implicit request =>
      Ok(views.html.personForm(personForm))
  }

  /**
   * Delete a [[Person]] identified by <em>id</em>.
   *
   * This method will ensure that all associations of the [[Person]] are deleted as well. i.e., [[Email]]
   * addresses, [[Phone]] numbers, [[Address]]es, and so on.
   *
   * @param id The identifier of the [[Person]] instance to be removed.
   */
  def delete(id: Long) = isAuthenticated { username =>
    implicit request =>
      Logger.info(s"The removal of the person with ID $id was requested.")
      val result = Person.deleteCascading(id)
      if (result.isSuccess) {
        Logger.logger.debug("Sucessfully deleted person with ID " + id + ".")
        Redirect(routes.PersonCtrl.list())
      } else {
        Logger.logger.error(result.toString, result.toEither.left.get)
        Redirect(routes.PersonCtrl.show(id))
      }
  }

  /**
   * Provide a list of all available person items.
   */
  def list = isAuthenticated { username =>
    implicit request =>
      val list: List[(String, List[Person])] = Nil

      val persons = Person.getAll
      if (persons.isSuccess) {
        (Messages("persons"), persons.toOption.get.sortBy(x => (x.lastname, x.firstname))) :: list
      } else {
        Logger.logger.error(persons.toString, persons.toEither.left.get)
        BadRequest("When trying to load the list of persons a failure occurred.")
      }

      Ok(views.html.personList(list, Messages("persons")))
  }

  /**
   * Load the person identified by <em>id</em> into the persond form for editing.
   *
   * @param id Identifier of the person to handle.
   */
  def edit(id: Long) = isAuthenticated { username =>
    implicit request =>
      val personV = Person.load(id)
      personV match {
        case Success(personOp) => personOp match {
          case Some(person) => Logger.logger.debug(s"Preparing editing of person with ID '$id'.")
            Ok(views.html.personForm(personForm.fill(person)))
          case None => Logger.logger.debug("Cannot find person with ID " + id + ".")
            Redirect(routes.PersonCtrl.list()).flashing("error" -> Messages("error.loading.person"))
        }
        case Failure(t) => Logger.error(personV.toString, t)
          Redirect(routes.PersonCtrl.list()).flashing("error" -> Messages("error.loading.person"))
      }
  }

  /**
   * Display the details of a person identified by <em>id</em>.
   *
   * @param id Identifier of the person to display.
   */
  def show(id: Long) = isAuthenticated { username =>
    implicit request =>
      val personV = Person.load(id)
      personV match {
        case Success(p) => p match {
          case Some(pers) =>
            Logger.logger.debug("Found person with ID " + id + ".")
            val paiV = PersonAdditionalInfo.load(id)
            val pai = paiV match {
              case Success(paiOp) => paiOp
              case Failure(_) => None
            }

            val tList = AcademicTitle.getPersonTitles(pers)
            val titles = tList match {
              case Success(list) => list
              case Failure(_) => Nil
            }

            val alist = Address.getPersonAddresses(pers)
            val adrs = alist match {
              case Success(list) => list
              case Failure(_) => Nil
            }

            val plist = Phone.getPersonPhones(pers)
            val phones = plist match {
              case Success(list) => list
              case Failure(_) => Nil
            }

            val elist = Email.getPersonEmails(pers)
            val emails = elist match {
              case Success(list) => list
              case Failure(_) => Nil
            }

            val hpList = Homepage.getPersonHomepages(pers)
            val hps = hpList match {
              case Success(list) => list
              case Failure(_) => Nil
            }

            Ok(views.html.person(pers, pai, titles, adrs, phones, emails, hps))
          case None =>
            Logger.logger.debug(s"Failed to load person with ID '$id'. Does not exist")
            Redirect(routes.PersonCtrl.list()).flashing("error" -> Messages("error.loading.person"))
        }
        case Failure(t) => Logger.error(personV.toString, t)
          Redirect(routes.PersonCtrl.list()).flashing("error" -> Messages("error.loading.person"))
      }
  }

  /**
   * Handle submitted form data.
   */
  def submit = isAuthenticated { username =>
    implicit request =>
      personForm.bindFromRequest.value map {
        p =>
          val result = Person.saveOrUpdate(p)
          if (result.isSuccess) {
            Redirect(routes.PersonCtrl.show(result.toOption.get.id.get))
          } else {
            Logger.logger.error(result.toString, result.toEither.left.get)
            Redirect(routes.PersonCtrl.list())
          }
      } getOrElse BadRequest
  }

  /**
   * Update person data.
   */
  def updatePerson() = isAuthenticated { username =>
    implicit request =>
      personForm.bindFromRequest.fold(
        errors => {
          Logger.error("An error occurred when trying to process the person form.")
          BadRequest(views.html.personForm(errors))
        },
        person => {
          Logger.debug("Storing person " + person.lastname + ", " + person.firstname.getOrElse(""))
          val result = Person.saveOrUpdate(person)
          if (result.isSuccess) {
            Redirect(routes.PersonCtrl.show(result.toOption.get.id.get))
          } else {
            Logger.error(result.toString, result.toEither.left.get)
            BadRequest(views.html.personForm(personForm.fill(person)))
          }
        })
  }
}