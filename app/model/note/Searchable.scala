package model.note

import model.html.Render

/**
  * Author: Sean
  * Date: 5/10/2017
  * Time: 10:02 AM
  */
abstract class Searchable extends Render {
  protected def renderHit(text: String, token: String): String = {
    text.replaceAll(token, "<strong class=\"text-danger\">" + token + "</strong>")
  }

  def search(tokens: List[String], context: Option[String]): List[Hit]
}
