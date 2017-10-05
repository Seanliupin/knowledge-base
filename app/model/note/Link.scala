package model.note

/**
  * Author: Sean
  * Date: 5/10/2017
  * Time: 10:33 AM
  */

abstract class Link(title: String, href: String, comment: String) extends Hitable {
  override def hit(token: String): Boolean = {
    title.toLowerCase.contains(token) || href.toLowerCase.contains(token) || comment.toLowerCase.contains(token)
  }
}

case class Web(title: String, href: String, comment: String) extends Link(title, href, comment)

case class Book(title: String, href: String, comment: String) extends Link(title, href, comment)

case class Line(line: String) extends Hitable {
  override def hit(token: String): Boolean = {
    line.toLowerCase.contains(token)
  }
}


object Extractor {
  val BookExtractor = """book:\s+\[(.*?)\]\((.*?)\)(.*)""" r
  val WebExtractor = """web:\s+\[(.*?)\]\((.*?)\)(.*)""" r
}