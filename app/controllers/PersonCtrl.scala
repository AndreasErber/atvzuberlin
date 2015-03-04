package controllers

import models.{ Person, Persons }
import util.CustomFormatters
import play.api.data.Form
import play.api.data.Forms._
import play.api.db._
import play.api.i18n.Messages
import play.api.Logger
import play.api.mvc._
import play.api.mvc.Security._
import play.api.Play.current
import models.Email
import controllers.ext.ProvidesCtx
import controllers.ext.Security
import models.Address
import models.Phone
import models.AcademicTitle
import models.PersonHasTitle
import models.Homepage
import models.PersonAdditionalInfo
import models.PersonHasEmails
import models.PersonHasEmail
import models.PersonHasHomepage
import models.PersonHasPhone
import models.AcademicTitles

/**
 * Controller to handle {@link Person} related requests.
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
   * Delete a {@link Person} identified by <em>id</em>.
   *
   * This method will ensure that all associations of the {@link Person} are deleted as well. i.e., {@link Email}
   * addresses, {@link Phone} numbers, {@link Address}es, and so on.
   *
   * @param id The identifier of the {@link Person} instance to be removed.
   */
  def delete(id: Long) = isAuthenticated { username =>
    implicit request =>
      Logger.info(s"The removal of the person with ID $id was requested.")
      val result = Person.deleteCascading(id)
      if (result.isSuccess) {
        Logger.logger.debug("Sucessfully deleted person with ID " + id + ".")
        Redirect(routes.PersonCtrl.list)
      } else {
        Logger.logger.error(result.toString(), result.toEither.left.get)
        Redirect(routes.PersonCtrl.show(id))
      }
  }

  /**
   * Provide a list of all available person items.
   */
  def list = isAuthenticated { username =>
    implicit request =>
      val list = Person.getAll
      if (list.isSuccess) {
        Ok(views.html.personList(list.toOption.get.sortBy(x => (x.lastname, x.firstname))))
      } else {
        Logger.logger.error(list.toString(), list.toEither.left.get)
        BadRequest("When trying to load the list of persons a failure occurred.")
      }
  }

  def edit(id: Long) = isAuthenticated { username =>
    implicit request =>
      val p = Person.load(id)
      p match {
        case None =>
          Logger.logger.debug("Cannot find person with ID " + id + "."); NotFound
        case Some(pers) =>
          Logger.logger.debug("Preparing editing of person with ID " + id + ".");
          Ok(views.html.personForm(personForm.fill(pers)))
        case _ => NotFound
      }
  }

  /**
   * Display the details of a person.
   */
  def show(id: Long) = isAuthenticated { username =>
    implicit request =>
      val p = Person.load(id)
      p match {
        case None =>
          Logger.logger.debug("No person with ID " + id + " found."); NotFound
        case Some(pers) =>
          Logger.logger.debug("Found person with ID " + id + ".")
          val pai = PersonAdditionalInfo.load(id)
          val tList = AcademicTitle.getPersonTitles(pers)
          val titles = if (tList.isSuccess) tList.toOption.get else Nil
          val alist = Address.getPersonAddresses(pers)
          val adrs = if (alist.isSuccess) alist.toOption.get else Nil
          val plist = Phone.getPersonPhones(pers)
          val phones = if (plist.isSuccess) plist.toOption.get else Nil
          val elist = Email.getPersonEmails(pers)
          val emails = if (elist.isSuccess) elist.toOption.get else Nil
          val hpList = Homepage.getPersonHomepages(pers)
          val hps = if (hpList.isSuccess) hpList.toOption.get else Nil
          Ok(views.html.person(pers, pai, titles, adrs, phones, emails, hps))
        case _ => NotFound
      }
  }

  def submit = isAuthenticated { username =>
    implicit request =>
      personForm.bindFromRequest.value map {
        p =>
          val result = Person.saveOrUpdate(p)
          if (result.isSuccess) {
            Redirect(routes.PersonCtrl.show(result.toOption.get.id.get))
          } else {
            Logger.logger.error(result.toString(), result.toEither.left.get)
            Redirect(routes.PersonCtrl.list)
          }

      } getOrElse BadRequest
  }

  def updatePerson = isAuthenticated { username =>
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
            Logger.error(result.toString(), result.toEither.left.get)
            BadRequest(views.html.personForm(personForm.fill(person)))
          }
        })
  }
}