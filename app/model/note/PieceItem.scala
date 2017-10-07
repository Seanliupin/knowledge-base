package model.note

import helper.StringUtil
import model.html.Node

/**
  * Author: Sean
  * Date: 5/10/2017
  * Time: 10:33 AM
  */
trait Hit {
  /**
    * todo: 不仅可以返回分数，还可以返回命中的个数，因此，更好的做法是返回一个tuple
    **/
  def hit(token: String): Int
}

trait Render {
  def toHtml(tokens: List[String]): String

  def toPlain: String

  /**
    * render hit with strong by default, sub class can render hit their style
    **/
  protected def renderHit(text: String, token: String): String = {
    text.replaceAll(token, Node(Some("strong"), token).className("text-danger"))
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


abstract class Link(title: String, href: String, comment: String) extends Paragraph(title) {
  protected def linkClassName: String = "piece-url"

  override def hit(token: String): Int = {
    List(title, href, comment).map(item => hitScore(item, token)).sum
  }

  override def toHtml(tokens: List[String]): String = {
    val url = Node(Some("a"), renderHits(title, tokens)).href(href).className(linkClassName)
    var comm = ""
    if (StringUtil.isNotBlank(comment)) {
      comm = Node(Some("div"), renderHits(comment, tokens)).className("piece-url-comment")
    }
    Node(Some("div"), url + comm).className("piece-url-container")
  }


  override def toPlain: String = s"[$title]($href)($comment)"
}

case class Web(title: String, href: String, comment: String) extends Link(title, href, comment) {
  override protected def linkClassName: String = "piece-web"

  override def paragraphType: Symbol = 'Web
}

case class Book(title: String, href: String, comment: String) extends Link(title, href, comment) {
  override protected def linkClassName: String = "piece-book"

  override def paragraphType: Symbol = 'Book
}

abstract class Paragraph(line: String) extends Hit with Render {
  override def hit(token: String): Int = {
    hitScore(line, token)
  }

  def isEmpty: Boolean = line.trim.length == 0

  /**
    * 计算命中分数，全字符命中得分最高
    * todo: 是否可以不用正则表达式来实现该算法
    **/
  final protected def hitScore(text: String, token: String): Int = {
    if (text.contains(token)) {
      return hitInWord
    }
    if (text.toLowerCase.contains(token.toLowerCase)) {
      return hitInWordIgnoreCase
    }

    return 0
  }

  final protected def hitWord: Int = weight * 5

  final protected def hitWordIgnoreCase: Int = weight * 3

  final protected def hitInWord: Int = weight * 2

  final protected def hitInWordIgnoreCase: Int = weight

  protected def weight: Int = Score.getScore(paragraphType)

  override def toPlain: String = line

  override def toString: String = line

  def paragraphType: Symbol

  def constrain(only: String): Boolean = true

  override def toHtml(tokens: List[String]): String = Node(Some("p"), renderHits(tokens)).className("piece-content")
}

case class Title(line: String) extends Paragraph(line) {
  def exist: Boolean = {
    line.trim.length > 0
  }

  def toHtml(tokens: List[String], fileName: String): String = Node(Some("a"), line).className("piece-title").title(fileName)

  override def paragraphType: Symbol = 'Title
}

case class SubTitle(line: String) extends Paragraph(line) {
  override def toHtml(tokens: List[String]): String = Node(Some("p"), renderHits(tokens)).className("piece-h3")

  override def paragraphType: Symbol = 'SubTitle
}

case class KeyWord(line: String) extends Paragraph(line) {
  override def paragraphType: Symbol = 'KeyWord

  override def toHtml(tokens: List[String]): String = Node(Some("code"), line).className("piece-keyword").toString()
}

case class Time(line: String) extends Paragraph(line) {
  override def toHtml(tokens: List[String]): String = Node(Some("text"), line).className("piece-time")

  override def paragraphType: Symbol = 'Time

}

case class Line(line: String) extends Paragraph(line) {
  override def paragraphType: Symbol = 'Line
}

case class Tip(line: String, tipType: Option[String]) extends Paragraph(line) {
  override def paragraphType: Symbol = 'Tip

  private def colorClass: String = {
    tipType match {
      case Some(c) => s"tip-$c"
      case None => "tip-default"
    }
  }

  override def constrain(only: String): Boolean = tipType == Some(only)

  override def toHtml(tokens: List[String]): String = Node(Some("p"), renderHits(tokens)).className("piece-tip").className(colorClass)
}

/**
  * 多行组成的一个块，比如一段代码，一段评注等。用某一个
  **/
abstract class Chapter(ctype: Option[String], title: Option[String]) extends Paragraph(title.getOrElse("")) {
  protected var lines: List[String] = List()

  def isValid: Boolean = ctype != None

  def addLine(line: String) = {
    lines = lines ++ List(line)
  }

  override def hit(token: String): Int = {
    //总命中分数由正文命中分数和标题命中分数构成，这里提升了标题命中分的权重
    lines.map(hitScore(_, token)).sum + hitScore(title.getOrElse(""), token) * 3
  }

  override def constrain(only: String): Boolean = ctype == Some(only)

  override def isEmpty: Boolean = {
    title.getOrElse("").length == 0 && lines.forall(_.trim.length == 0)
  }

  /**
    * 子类可以自己渲染其内容，比如代码段可以自行根据语言类型进行渲染
    **/
  protected def renderedBody(tokens: List[String]): String = {
    val base = new StringBuilder
    lines.foreach(code => {
      base.append(Node(Some("div"), renderHits(code, tokens)).className("chapter-line"))
    })

    base.toString()
  }

  override def toHtml(tokens: List[String]): String = {
    var titleNode = ""
    title match {
      case Some(t) => {
        if (t.trim.length > 0) {
          titleNode = Node(Some("div"), t).className("chapter-title")
        }
      }
      case None =>
    }

    val bodyNode = Node(Some("div"), renderedBody(tokens)).className("chapter-body")

    Node(Some("div"), titleNode + bodyNode).className("chapter")
  }

}

case class Code(lan: Option[String], title: Option[String]) extends Chapter(lan, title) {

  override def paragraphType: Symbol = 'Code

  override def renderedBody(tokens: List[String]): String = {
    val base = new StringBuilder
    lines.foreach(code => {
      base.append(Node(Some("div"), renderHits(code, tokens)).className("code-line"))
    })
    base.toString
  }
}

case class Comment(ctype: Option[String], title: Option[String]) extends Chapter(ctype, title) {

  override def paragraphType: Symbol = 'Comment

  override def renderedBody(tokens: List[String]): String = {
    val base = new StringBuilder
    lines.foreach(code => {
      base.append(Node(Some("div"), renderHits(code, tokens)).className("comment-line"))
    })
    base.toString
  }
}

object Time {
  implicit def stringToTime(line: String) = Time(line)
}

object Title {
  implicit def stringTitle(line: String) = Title(line)
}


object Extractor {
  val titleExtractor = """##\s+(.*)""" r
  val subTitleExtractor = """###\s+(.*)""" r
  val keysExtractor = """keys:\s+(.*)""" r
  val tagsExtractor = """tags:\s+(.*)""" r
  val WebExtractor = """web:\s+\[(.*?)\]\((.*?)\)[,，。.]?(.*)""" r
  val bookExtractor = """book:\s+\[(.*?)\]\((.*?)\)[,，。.]?(.*)""" r
  val WebItemExtractor = """\s*[*]?\s*\[(.*?)\]\((.*?)\)[,，。.]?(.*)""" r
  val timeExtractor = """time:\s+(.*)""" r
  val typeLessTipExtractor = """>(.*)""" r
  val typedTipExtractor = """>(.*?):\s*(.*)""" r
  val codeHeaderExtractor = """```(\w*)[:]?\b*(.*)""" r
  val commentHeaderExtractor = """'''(\w*)[:]?\b*(.*)""" r
  val codeFooterExtractor = """```""" r
  val commentFooterExtractor = """'''""" r
}