package model.note

/**
  * Author: Sean
  * Date: 5/10/2017
  * Time: 4:45 PM
  */
case class ContextOption(value: String, select: Boolean) {
  def selected: String = {
    if (select) {
      "selected"
    } else {
      ""
    }
  }
}