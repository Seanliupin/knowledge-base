package model.note

import helper.StringUtil
import model.html.Node

/**
  * Author: Sean
  * Date: 5/10/2017
  * Time: 10:33 AM
  */
trait Hit {
  def hit(token: String): Boolean
}

trait Render {
  def toHtml(tokens: List[String]): String

  def toPlain: String

  /**
    * render hit with strong by default, sub class can render hit their style
    **/
  protected def renderHit(text: String, token: String): String = {
    text.replaceAll(token, Node("strong", token).className("text-danger"))
  }

  protected def renderHits(text: String, tokens: List[String]): String = {
    var renderedText = text
    tokens.foreach(token => {
      renderedText = renderHit(renderedText, token)
    })
    renderedText
  }

  final def renderHits(tokens: List[String]): String = {
    renderHits(toPlain, tokens)
  }
}


abstract class Link(title: String, href: String, comment: String) extends Hit with Render {
  override def hit(token: String): Boolean = {
    val lowerToken = token.toLowerCase
    List(title, href, comment).exists(item => item.toLowerCase.contains(lowerToken))
  }

  protected def linkClassName: String = "piece-url"

  override def toHtml(tokens: List[String]): String = {
    val url = Node("a", renderHits(title, tokens)).href(href).className(linkClassName)
    var comm = ""
    if (StringUtil.isNotBlank(comment)) {
      comm = Node("div", renderHits(comment, tokens)).className("piece-url-comment")
    }
    Node("div", url + comm).className("piece-url-container")
  }


  override def toPlain: String = s"[$title]($href)($comment)"
}

case class Web(title: String, href: String, comment: String) extends Link(title, href, comment) {
  override protected def linkClassName: String = "piece-web"
}

case class Book(title: String, href: String, comment: String) extends Link(title, href, comment) {
  override protected def linkClassName: String = "piece-book"
}


abstract class Paragraph(line: String) extends Hit with Render {
  override def hit(token: String): Boolean = {
    line.toLowerCase.contains(token.toLowerCase)
  }

  override def toPlain: String = line

  override def toString: String = line

  override def toHtml(tokens: List[String]): String = Node("p", renderHits(tokens)).className("piece-content")
}

case class Title(line: String) extends Paragraph(line) {
  def exist: Boolean = {
    line.trim.length > 0
  }

  def toHtml(tokens: List[String], fileName: String): String = Node("a", line).className("piece-title").title(fileName)
}

object Title {
  implicit def stringTitle(line: String) = Title(line)
}

case class SubTitle(line: String) extends Paragraph(line){
  override def toHtml(tokens: List[String]): String = Node("p", renderHits(tokens)).className("piece-h3")
}


case class KeyWord(line: String) extends Paragraph(line)

object KeyWord {
  implicit def stringToKeyWord(line: String) = KeyWord(line)
}

case class Time(line: String) extends Paragraph(line) {
  override def toHtml(tokens: List[String]): String = Node("text", line).className("piece-time")
}

object Time {
  implicit def stringToTime(line: String) = Time(line)
}

case class Line(line: String) extends Paragraph(line)


case class Comment(line: String) extends Paragraph(line) {
  override def toHtml(tokens: List[String]): String = Node("p", renderHits(tokens)).className("piece-comment")
}

object Comment {
  implicit def stringToComment(line: String) = Comment(line)
}

case class Code(language: String) extends Paragraph(language){
  var codes: List[String] = List()

  def addCodeLine(code: String) = {
    codes = codes ++ List(code)
  }

  def hasCode: Boolean = codes.exists(_.trim.length > 0)

}

object Extractor {
  val titleExtractor = """##\s+(.*)""" r
  val subTitleExtractor = """###\s+(.*)""" r
  val keysExtractor = """keys:\s+(.*)""" r
  val tagsExtractor = """tags:\s+(.*)""" r
  val WebExtractor = """web:\s+\[(.*?)\]\((.*?)\)(.*)""" r
  val WebItemExtractor = """\s*[*]\s+\[(.*?)\]\((.*?)\)(.*)""" r
  val timeExtractor = """time:\s+(.*)""" r
  val commentExtractor = """comment:\s+(.*)""" r
  val bookExtractor = """book:\s+\[(.*?)\]\((.*?)\)(.*)""" r
}