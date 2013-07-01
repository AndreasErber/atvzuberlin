/**
 *
 */
package controllers

import play.api.mvc.Controller
import controllers.ext.ProvidesCtx
import controllers.ext.Security
import play.api.data.Form
import play.api.data.Forms._
import play.api.Logger
import models.Sports
import models.SportsDate
import util.CustomFormatters

/**
 * @author andreas
 * @version 0.0.1, 2013-06-30
 */
class SportCtrl extends Controller with ProvidesCtx with Security {

  implicit val sqlTimeFormatter = CustomFormatters.sqlTimeFormatter

  val sqlTimeMapping = of[java.sql.Time]

  val sportsForm = Form[Sports](
    mapping(
      "id" -> optional(longNumber),
      "title" -> text,
      "description" -> optional(text),
      "created" -> longNumber,
      "creator" -> text,
      "modified" -> optional(longNumber),
      "modifier" -> optional(text))(Sports.apply)(Sports.unapply))

  val sportsDatesForm = Form[SportsDate](
    mapping(
      "id" -> optional(longNumber),
      "sport" -> longNumber,
      "locationName" -> optional(text),
      "locationStreet" -> optional(text),
      "locationZip" -> optional(text),
      "locationCity" -> optional(text),
      "time" -> optional(sqlTimeMapping),
      "created" -> longNumber,
      "creator" -> text,
      "modified" -> optional(longNumber),
      "modifier" -> optional(text))(SportsDate.apply)(SportsDate.unapply))
}