package model.note

import model.html.{Node, Render}

/**
  * Author: Sean
  * Date: 5/10/2017
  * Time: 10:33 AM
  */

abstract class Link(title: String, href: String, comment: String) extends Hitable with Render {
  override def hit(token: String): Boolean = {
    val lowerToken = token.toLowerCase
    List(title, href, comment).exists(item => item.toLowerCase.contains(lowerToken))
  }

  override def toHtml(tokens: List[String]): String = Node("a", renderHits(title, tokens)).href(href).className("piece-web").toString()

  override def toPlain: String = s"[$title]($href)($comment)"
}

case class Web(title: String, href: String, comment: String) extends Link(title, href, comment)

case class Book(title: String, href: String, comment: String) extends Link(title, href, comment)

abstract class Paragraph(line: String) extends Hitable with Render {

  override def hit(token: String): Boolean = {
    line.toLowerCase.contains(token.toLowerCase)
  }

  override def toPlain: String = line

  override def toString: String = line

  override def toHtml(tokens: List[String]): String = Node("p", renderHits(tokens)).className("piece-content").toString()
}


case class Line(line: String) extends Paragraph(line) {
}

case class Comment(line: String) extends Paragraph(line) {
  override def toHtml(tokens: List[String]): String = Node("p", renderHits(tokens)).className("piece-comment").toString()
}

object Line {
  implicit def stringToLine(line: String) = Line(line)
}

object Comment {
  implicit def stringToComment(line: String) = Comment(line)
}

object Category {
  def title = "## "

  def keys = "keys: "

  def tags = "tags: "

  def web = "web: "

  def time = "time: "

  def comment = "comment: "
}


object Extractor {
  val titleExtractor = """##\s+(.*)""" r
  val subTitleExtractor = """###\s+(.*)""" r
  val keysExtractor = """keys:\s+(.*)""" r
  val tagsExtractor = """tags:\s+(.*)""" r
  val WebExtractor = """web:\s+\[(.*?)\]\((.*?)\)(.*)""" r
  val timeExtractor = """time:\s+(.*)""" r
  val commentExtractor = """comment:\s+(.*)""" r

  val BookExtractor = """book:\s+\[(.*?)\]\((.*?)\)(.*)""" r
}