/**
 *
 */
package models

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import play.api.db.BoneCPPlugin
import play.api.db.DB
import play.api.Logger
import play.api.test._
import play.api.test.Helpers._
import play.api.test.WithApplication
import org.specs2.mutable._
import org.scalatest._

/**
 * @author andreas
 *
 */
class PersonSpec extends Specification {

  val app = FakeApplication()

  var id: Long = -1
  val lastname = "Dow"
  val firstname = Option("John")
  val creator = "TestCase"
  val nick = "Dowie"

  "A Person" should {
    "be creatable and updatable" in new WithApplication(app) {

      // insert a new person
      val p = Person.saveOrUpdate(Person(None, lastname, firstname, creator = creator))
      if (p.isSuccess) {
        assert(p.toOption.get.id !== None)
      } else {
    	  assert(1 == 2)
      }
      id = p.toOption.get.id.get

      val loaded = Person.load(id)
      loaded should not be (None)
      assert(loaded.get === p)
      Logger.logger.info("Successfully created a person entry.")

      // set a nickname
      val p1 = loaded.get.copy(nickname = Some(nick))
      val p2 = Person.saveOrUpdate(p1)
      if (p2.isSuccess) {
        Logger.logger.info("Successfully updated a person entry.")
        assert(p1.id.get === id)
        assert(p2.toOption.get.id.get === p1.id.get)
        assert(p2.toOption.get.nickname.get === nick)
      } else {
        Logger.logger.error("Failed to update a person entry.")
        assert(1 == 2)
      }

    }
  }

}