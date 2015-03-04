/**
 *
 */
package controllers

import accesscontrol.User
import accesscontrol.UserHasRoles
import controllers.ext.ProvidesCtx
import controllers.ext.Security
import exception.PasswordException
import java.security.MessageDigest
import models.Email
import models.Person
import models.Persons
import models.Registration
import models.Registrations
import play.api._
import play.api.data._
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import play.api.i18n.Messages
import play.api.Logger
import play.api.mvc.Action
import play.api.mvc.Controller
import play.api.mvc.Flash
import play.api.mvc.Security
import play.api.Play.current
import scala.util.Random
import scalaz.Failure
import scalaz.Success
import scalaz.Validation
import util.Business
import util.Ctx
import util.EncryptionUtil
import util.Honorary
import util.KV
import util.Personal
import util.UserRole

/**
 * Controller to handle requests that pertain a user, e.g. login, logout, registration, ...
 *
 * @author andreas
 * @version 0.0.9, 2015-01-12
 */
object UserCtrl extends Controller with ProvidesCtx with Security {

  val registrationPasswordForm = Form(
    tuple(
      "link" -> nonEmptyText,
      "username" -> nonEmptyText,
      "password1" -> nonEmptyText,
      "password2" -> nonEmptyText) //      .verifying(Messages("registration.error.password.too.short"), t => (t._2.length() > 8))
      //      .verifying(Messages("registration.error.password.confirmation.failed"), t => t._2 == t._3))
      )
  val usernameSelectionForm = Form(single("nickname" -> nonEmptyText))

  val loginForm = Form(
    tuple(
      "username" -> nonEmptyText,
      "password" -> nonEmptyText) //      verifying (Messages("error.login.failed"), fields => fields match {
      //        case (e, p) => User.auth(e, this.encryptPassword(p)).toOption.isDefined
      //      })
      )

  val subject = "Deine Registrierung auf http://www.atvzuberlin.de"
  val emailSender = "ATV zu Berlin Webmaster <webmaster@atvzuberlin.de>"
  def bodyText(salutation: String, username: String, regLink: String) = s"""$salutation $username,
    
    vielen Dank, dass du dich auf http://www.atvzuberlin.de registrieren möchtest. 
    Um den Vorgang fortzusetzen öffne bitte den folgenden Link in einem Browser
    
    $regLink
    
    Dort gibst du dann bitte ein Passwort ein und schließt damit die Registrierung ab.
    
    Bitte beachte, dass der Registrierungslink nach seiner Erzeugung nur 2 Tage gültig
    ist. Verwendest du ihn innerhalb dieser Zeit nicht, so musst du den Vorgang hier
    
    http://www.atvzuberlin.de/registration
    
    erneut starten.
    
    
    Mit bbrbbrlichen Grüßen
    Der Webmaster
    """

  def displayLoginForm() = Action { implicit request =>
    Logger.debug("Display login form")
    Ok(views.html.login(loginForm, None))
  }

  def index() = Action { implicit request =>
    Ok(views.html.index("Des is die Messetsch!"))
  }

  /**
   * Trying to log in a user.
   */
  def login() = Action { implicit request =>
    Logger.debug("Authenticating user ...")
    // binding will evaluate the credentials against the DB
    loginForm.bindFromRequest.fold(
      hasErrors = { form =>
        Logger.error(s"Authentication of user '${form.data.get("username").get}' failed: " + form.errors)
        Redirect(routes.UserCtrl.index).withNewSession.flashing(Flash(form.data) + ("error" -> Messages("error.login.failed")))
      },
      tuple => {
        val username = tuple._1
        val password = this.encryptPassword(tuple._2)
        val result = User.auth(username, password)
        if (result.isSuccess) {
          Logger.debug(s"Authentication of user '$username' was successful!")
          Redirect(getCtxt.referer.get).withSession(Security.username -> username).flashing("success" -> Messages("login.successful"))
        } else {
          Logger.debug(s"Authentication of user '$username' failed.", result.toEither.left.get)
          Redirect(routes.UserCtrl.displayLoginForm).flashing("error" -> Messages("error.login.failed"))
        }
      })
  }

  /**
   * Log out the current user.
   */
  def logout = Action { implicit request =>
    Redirect("/").withNewSession.flashing("success" -> Messages("logout.successful"))
  }

  /**
   * Display the registration form to allow a user to register.
   */
  def showUsernameSelectionForm = Action { implicit request =>
    Logger.debug("Display registration form")
    // get all persons that are eligible for registration: Members of ATV
    val memberStates = KV.getAtvMemberStates()
    val personListVal = Person.getAllByStatus(memberStates.toList)
    if (personListVal.isFailure) {
      Logger.error("Registration is not possible - Retrieval of persons failed: " + personListVal.toEither.left.get.getMessage())
      Redirect("/").flashing("error" -> Messages("error.registration.not.available"))
    }
    val personList = personListVal.getOrElse(Nil)
    var personListFiltered = List[Person]()
    val userListVal = User.getAll;
    if (userListVal.isSuccess) {
      // remove the persons that are already registered from the offered list
      personListFiltered = personList diff userListVal.toOption.get.map(u => u.person)
    } else {
      Logger.error("Failed to load list of users and diff it from the list of members for registration.")
      personListFiltered = personList
    }
    Ok(views.html.registrationForm(usernameSelectionForm, personListFiltered.sortBy(p => p.nickname)))
  }

  def register = Action { implicit request =>
    Logger.debug("Registering user ...")
    // binding will evaluate the credentials against the DB
    registrationPasswordForm.bindFromRequest.fold(
      formWithErrors => {
        Logger.error("Registration failed after form data validation: " + formWithErrors.errors)
        BadRequest(views.html.register(formWithErrors))
      },
      tuple => {
        Logger.debug("Form data valdiation successful.")
        val link = tuple._1
        val username = tuple._2
        val pw1 = tuple._3
        val pw2 = tuple._4
        val registrationVal = Registration.getByUsername(username)
        if (registrationVal.isFailure) {
          Logger.error(s"Failed to find matching registration for username '${username}': " + registrationVal.toEither.left.get.getMessage())
          Redirect(routes.UserCtrl.registrationConfirmation(link)).flashing("error" -> Messages("error.registration.for.username.not.found"))
        } else {
          Logger.debug(s"Found matching registration for username $username")
          val personVal = Person.getByNickname(username)
          if (personVal.isFailure) {
            Logger.error(s"Failed to find matching person for username '$username': " + registrationVal.toEither.left.get.getMessage())
            Redirect(routes.UserCtrl.registrationConfirmation(link)).flashing("error" -> Messages("error.registration.person.not.found"))
          } else {
            Logger.debug(s"Found matching person for username '$username'")
            val person = personVal.toOption.get
            val registration = registrationVal.toOption.get

            val passwordVal = this.validateAndEncryptPassword(pw1, pw2);
            if (passwordVal.isFailure) {
              val errMsg = passwordVal.toEither.left.get.getMessage()
              Logger.error(s"Password validation failed for user '$username': " + errMsg)
              Redirect(routes.UserCtrl.registrationConfirmation(link)).flashing("error" -> errMsg)
            } else {
              Logger.debug("Password validation was successful.")
              val u = User(username, passwordVal.toOption.get, registration.email, person, System.currentTimeMillis(), None)
              val result = User.insert(u)
              if (result.isSuccess) {
                Logger.debug(s"Registration of user '$username' was successful.")
                val delRegVal = Registration.delete(registration.id.get)
                if (delRegVal.isFailure) {
                  Logger.error(s"Deletion of registration with ID ${registration.id.get} failed: " + delRegVal.toEither.left.get.getMessage())
                } else {
                  Logger.debug(s"Deletion of registration with ID ${registration.id.get} was successful.")
                }
                Redirect(routes.UserCtrl.index).flashing("success" -> Messages("success.registrationSuccessful"))
              } else {
                Logger.error(s"Registration of user ${u.username} failed!", result.toEither.left.get)
                Redirect(routes.UserCtrl.registrationConfirmation(link)).flashing("error" -> Messages("error.registrationFailed"))
              }
            }
          }
        }
      })
  }

  private def validateAndEncryptPassword(pw1: String, pw2: String): Validation[Throwable, String] = {
    if (pw1 != pw2) {
      return Failure(new PasswordException(Messages("error.passwords.not.equal")))
    }

    if (pw1.length() < User.minimumPasswordLength) {
      return Failure(new PasswordException(Messages("error.password.too.short", User.minimumPasswordLength)))
    }

    val specialChars = """!?,;.:()=+\-§$%&/#"""
    val atLeastOneDigitLookahead = """^(?=.*\d{1,}).{8,}$""".r
    val atLeastOneLowerCaseLetterLookahead = """^(?=.*[a-z]{1,}).{8,}$""".r
    val atLeastOneUpperCaseLetterLookahead = """^(?=.*[A-Z]{1,}).{8,}$""".r
    val atLeastOneSpecialCharLookahead = ("""^(?=.*[""" + specialChars + """]{1,}).{8,}$""").r

    if (!atLeastOneDigitLookahead.findFirstIn(pw1).isDefined) {
      return Failure(new PasswordException(Messages("error.password.missing.digit")))
    }
    if (!atLeastOneLowerCaseLetterLookahead.findFirstIn(pw1).isDefined) {
      return Failure(new PasswordException(Messages("error.password.missing.lower.case.letter")))
    }
    if (!atLeastOneUpperCaseLetterLookahead.findFirstIn(pw1).isDefined) {
      return Failure(new PasswordException(Messages("error.password.missing.upper.case.letter")))
    }
    if (!atLeastOneSpecialCharLookahead.findFirstIn(pw1).isDefined) {
      return Failure(new PasswordException(Messages("error.password.missing.special.char", specialChars.replace("""\""", ""))))
    }

    Success(this.encryptPassword(pw1))
  }

  private def encryptPassword(password: String): String = {
    EncryptionUtil.encrypt(password)
  }

  /**
   * Action to take when the username selection form is submitted.
   */
  def registration = Action { implicit request =>
    Logger.debug("Received registration request")
    usernameSelectionForm.bindFromRequest.fold(
      formWithErrors => {
        Logger.error("Registration failed due to: " + formWithErrors.errors.mkString(","))
        BadRequest(views.html.registrationForm(formWithErrors, Person.getAll.toOption.getOrElse(Nil)))
      },
      nickname => {
        val existingUserVal = User.findByName(nickname)
        if (existingUserVal.isSuccess && existingUserVal.toOption.isDefined) {
          Logger.error(s"Registration of user $nickname failed: Username is already in use.")
          Redirect(routes.UserCtrl.showUsernameSelectionForm()).flashing("error" -> Messages("error.username.already.registered"))
        } else {

          val personVal = Person.getByNickname(nickname)
          if (personVal.isFailure) {
            Logger.error(s"Failed to find person with nickname '$nickname': " + personVal.toEither.left.get.getMessage())
            Redirect(routes.UserCtrl.showUsernameSelectionForm()).flashing("error" -> Messages("error.username.unknown"))
          } else {

            Logger.logger.debug(s"Found matching person for nickname '$nickname': ")
            val p = personVal.toOption.get
            val eMailVal = this.provideSuitablePersonEmail(p)
            if (eMailVal.isFailure) {
              Logger.logger.error(s"Failed to find email addresses for person with nickname '$nickname': " + eMailVal.toEither.left.get.getMessage())
              Redirect(routes.UserCtrl.showUsernameSelectionForm).flashing("error" -> Messages("error.registration.no.email"))
            } else {
              val email = eMailVal.toOption.get
              val hash = this.createHash(p)
              val reg = Registration(None, p.nickname.get, hash, email, System.currentTimeMillis() + 172800, System.currentTimeMillis(), p.nickname.get, None, None)
              val result = Registration.insert(reg)

              this.sendMail(p, email, hash)
              Redirect("/").flashing("success" -> Messages("registration.email.sent.successfully"))
            }
          }
        }
      })

  }

  /**
   * Retrieve a suitable {@link Email} address for the given person.
   *
   * The method will first try to return a private email address. If none is available an honorary is attempted.
   * If that fails a business email address is tried.
   *
   * @param p The {@link Person} instance to find a suitable {@link Email} address for.
   * @returns A {@link Validation} that holds a suitable {@link Email} address or the {@link Throwable} in case of
   *          failure.
   */
  private def provideSuitablePersonEmail(p: Person): Validation[Throwable, Email] = {
    val personEmailsVal = Email.getPersonEmails(p)
    if (personEmailsVal.isFailure) {
      Logger.error(s"Failed to load emails for person ${p.fullname}: " + personEmailsVal.toEither.left.get.getMessage())
      return Failure(new RuntimeException(Messages("error.registration.no.email"), personEmailsVal.toEither.left.get))
    }

    val personEmails = personEmailsVal.getOrElse(Nil)
    if (personEmails.isEmpty) {
      return Failure(new NoSuchElementException(Messages("error.registration.no.email")))
    }

    // retrieve first private email, in case of failure use business email
    val personPrivateEmails = personEmails.filter(e => e._2.usage == Personal).map(e => e._1)
    if (!personPrivateEmails.isEmpty) {
      return Success(personPrivateEmails(0))
    }
    Logger.error(s"Found no private email address for person '${p.fullname}'")

    val personHonoraryEmails = personEmails.filter(e => e._2.usage == Honorary).map(e => e._1)
    if (!personHonoraryEmails.isEmpty) {
      return Success(personHonoraryEmails(0))
    }
    Logger.error(s"Found no honorary email address for person '${p.fullname}'")

    val personBusinessEmails = personEmails.filter(e => e._2.usage == Business).map(e => e._1)
    if (!personBusinessEmails.isEmpty) {
      return Success(personBusinessEmails(0))
    }
    Logger.error(s"Found no business email address for person '${p.fullname}'")

    Failure(new NoSuchElementException(Messages("error.registration.no.email")))
  }

  /**
   * Create a unique hash from the {@link Person}'s full name and the current time.
   */
  private def createHash(p: Person): String = {
    val rand = new Random(System.currentTimeMillis())
    val charStream = rand.alphanumeric.take(64)
    new String(charStream.toArray)
  }

  private def sendMail(p: Person, email: Email, hash: String) = {
    val salutation = p.gender match {
      case 'f' => "Liebe"
      case 'm' => "Lieber"
    }
    val regLink = "http://www.atvzuberlin.de/regConf/" + hash
    val to = Seq(p.firstname.getOrElse("") + " " + p.lastname + " <" + email.address + ">")
    val emailMsg = play.api.libs.mailer.Email(subject, emailSender, to, bodyText = Some(this.bodyText(salutation, p.nickname.get, regLink)))
    play.api.libs.mailer.MailerPlugin.send(emailMsg)
  }

  def registrationConfirmation(link: String) = Action { implicit request =>
    Logger.debug("Received a registration confirmation request.")
    if (Option(link).isDefined && !link.isEmpty()) {
      Logger.debug("The link is defined")
      val registrationVal = Registration.getByLink(link);
      if (registrationVal.isFailure) {
        Logger.error(s"A registration with the link '$link' could not be found: " + registrationVal.toEither.left.get)
        Redirect("/").flashing("error" -> Messages("error.registration.for.link.not.found"))
      } else {
        val username = registrationVal.toOption.get.username
        Logger.debug(s"The registration confirmation request belongs to username '$username'.")
        val expires = registrationVal.toOption.get.expires
        if (expires < System.currentTimeMillis()) {
          Logger.error(s"The registration with link $link has timed out.")
          Redirect(routes.UserCtrl.showUsernameSelectionForm).flashing("error" -> Messages("error.registration.timed.out"))
        } else {
          Ok(views.html.register(registrationPasswordForm.fill((link, username, "", ""))))
        }
      }
    } else {
      Redirect("/")
    }
  }

  /**
   * Provide a list of all available role items.
   */
  def list = isAuthenticated { username =>
    implicit request =>
      val list = User.getAll
      if (list.isSuccess) {
        Ok(views.html.usersList(list.toOption.get.sortBy(x => x.username)))
      } else {
        Logger.error(list.toString(), list.toEither.left.get)
        BadRequest("When trying to load the list of users a failure occurred.")
      }
  }

  def show(un: String) = isAuthenticated { username =>
    implicit request =>
      val user = User.findByName(un)
      if (user.isSuccess) {
        val uhr = UserHasRoles.getByUser(user.toOption.get)
        if (uhr.isSuccess) {
          Ok(views.html.user(user.toOption.get, uhr.toOption.get))
        } else {
          Logger.logger.error("Failed to retrieve user roles for user '" + un + "'.", uhr.toEither.left.get)
          Redirect(routes.UserCtrl.list).flashing("error" -> Messages("error.failedToFindUserRoles", un))
        }
      } else {
        Logger.logger.error("Failed to retrieve user named '" + un + "'.", user.toEither.left.get)
        Redirect(routes.UserCtrl.list).flashing("error" -> Messages("error.failedToFindUserByName", un))
      }
  }

  private def getUsernameSelection = {
    val list = Person.getAll
    if (list.isSuccess) {
      for (p <- list.toOption.get) yield (p.id.get.toString, p.lastname + " - " + p.nickname.getOrElse(""))
    } else {
      Nil
    }
  }
}