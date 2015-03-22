package controllers.ext

import models.Person
import util.MemberState
import play.api.i18n.Messages
import play.api.Logger

trait SublistRetrieverAndAdder {

  def getAndAddListOfMembersByStatus(list: List[(String, List[Person])], title: String, status: MemberState): List[(String, List[Person])] = {
    val sublist = Person.getAllByStatus(status)
      if (sublist.isSuccess) {
        return (Messages(title), sublist.toOption.get.sortBy(x => (x.lastname, x.firstname))) :: list
      } else {
        Logger.logger.error(sublist.toString(), sublist.toEither.left.get)
      }
      list
  }
}