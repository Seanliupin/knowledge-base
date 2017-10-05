package model.note

/**
  * Author: Sean
  * Date: 5/10/2017
  * Time: 10:33 AM
  */

abstract class Link(title: String, link: String, comment: String)

case class Web(title: String, link: String, comment: String) extends Link(title, link, comment)

case class Book(title: String, link: String, comment: String) extends Link(title, link, comment)

object Extractor {
  val BookExtractor = """book:\s+\[(.*?)\]\((.*?)\)(.*)""" r
  val WebExtractor = """web:\s+\[(.*?)\]\((.*?)\)(.*)""" r
}