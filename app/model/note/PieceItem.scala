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
    * */
  def hit(token: String): Int
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


abstract class Link(title: String, href: String, comment: String) extends Paragraph(title) {
  protected def linkClassName: String = "piece-url"

  override def hit(token: String): Int = {
    List(title, href, comment).map(item => hitScore(item, token)).sum
  }

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

  override def toHtml(tokens: List[String]): String = Node("p", renderHits(tokens)).className("piece-content")
}

case class Title(line: String) extends Paragraph(line) {
  def exist: Boolean = {
    line.trim.length > 0
  }

  def toHtml(tokens: List[String], fileName: String): String = Node("a", line).className("piece-title").title(fileName)

  override def paragraphType: Symbol = 'Title
}

case class SubTitle(line: String) extends Paragraph(line) {
  override def toHtml(tokens: List[String]): String = Node("p", renderHits(tokens)).className("piece-h3")

  override def paragraphType: Symbol = 'SubTitle
}

case class KeyWord(line: String) extends Paragraph(line) {
  override def paragraphType: Symbol = 'KeyWord

  override def toHtml(tokens: List[String]): String = Node("code", line).className("piece-keyword").toString()
}

case class Time(line: String) extends Paragraph(line) {
  override def toHtml(tokens: List[String]): String = Node("text", line).className("piece-time")

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

  override def toHtml(tokens: List[String]): String = Node("p", renderHits(tokens)).className("piece-tip").className(colorClass)
}

case class Code(language: Option[String]) extends Paragraph(language.getOrElse("")) {
  var codes: List[String] = List()

  def addCode(code: String) = {
    codes = codes ++ List(code)
  }

  override def hit(token: String): Int = {
    codes.map(hitScore(_, token)).sum
  }

  def isValidCode: Boolean = language != None

  def hasCode: Boolean = codes.exists(_.trim.length > 0)

  override def constrain(only: String): Boolean = language == Some(only)

  override def paragraphType: Symbol = 'Code

  override def toHtml(tokens: List[String]): String = {
    val base = new StringBuilder
    codes.foreach(code => {
      base.append(Node("div", renderHits(code, tokens)).className("code-line"))
    })

    Node("figure", "").className("highlight")
      .setText(Node("pre", "")
        .setText(Node("code", base.toString)
          .className("language-html")
          .addProperty("data-lang", language.getOrElse("html"))))

  }

  override def isEmpty: Boolean = codes.size == 0
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
  val codeHeaderExtractor = """```(.*)""" r
  val codeFooterExtractor = """```""" r
  val commentHeaderExtractor = """'''(.*)""" r
  val commentFooterExtractor = """'''""" r
}