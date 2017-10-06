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

  override def toString: String = line
}

object Line {
  implicit def stringToLine(line: String) = Line(line)
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