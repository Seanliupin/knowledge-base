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
  def hit(token: String): List[(Boolean, Boolean, Int, Symbol)]
}

trait Render {
  def toHtml(tokens: List[String]): String

  def toPlain: String

  /**
    * render hit with strong by default, sub class can render hit their style
    **/
  protected def renderHit(text: String, token: String): String = {
    val _token = token.map(c => {
      if (c.isLetter) {
        s"(${c.toUpper}|${c.toLower})"
      } else if ("()[]{}+.*?".contains(c)) {
        s"\\${c}"
      } else {
        c
      }
    }).mkString("")

    text.replaceAll(_token, Node(Some("strong"), token).className("text-danger"))

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

  override def hit(token: String): List[(Boolean, Boolean, Int, Symbol)] = {
    List(title, href, comment).flatMap(item => hitScore(item, token))
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
  override def hit(token: String): List[(Boolean, Boolean, Int, Symbol)] = {
    hitScore(line, token)
  }

  def isEmpty: Boolean = line.trim.length == 0

  /**
    * 计算命中分数，全字符命中得分最高
    * todo: 是否可以不用正则表达式来实现该算法
    **/
  final protected def hitScore(text: String, token: String): List[(Boolean, Boolean, Int, Symbol)] = {
    getMatchList(token, text).map(x => (x._1, x._2, x._3, paragraphType))
  }

  /**
    * List[(是否全字符匹配, 是否大小写匹配，单词位置)]
    **/
  private def getMatchList(word: String, target: String): List[(Boolean, Boolean, Int)] = {
    val trimWord = word.trim
    val ignoreCase = trimWord.map(c => {
      if (c.isLetter) {
        s"(${c.toLower}|${c.toUpper})"
      } else if ("()[]{}+.*?".contains(c)) {
        s"\\$c"
      } else {
        c
      }
    }).foldLeft("")((a, b) => {
      a + b
    })

    val ignoreCaseR = s"${ignoreCase}" r
    val paddingTarget = s" ${target} " //保证了获取字符的时候不越界
    ignoreCaseR
      .findAllMatchIn(paddingTarget)
      .map(w => (w, paddingTarget.charAt(w.start - 1), paddingTarget.charAt(w.end), w.start - 1))
      .map(tu => (!(tu._2.isLetterOrDigit || tu._3.isLetterOrDigit), tu._1.toString == trimWord, tu._4))
      .toList
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

case class Title(line: String, hrefId: Option[String] = None) extends Paragraph(line) {
  def exist: Boolean = {
    line.trim.length > 0
  }

  def toHtml(tokens: List[String], fileName: String): String = {

    val titleNode = Node(Some("a"), line).className("piece-title").title(fileName)
    hrefId match {
      case Some(href) => Node(Some("div"), titleNode).addProperty("id", href)
      case None => titleNode
    }
  }

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
  private val itemExtractor = """\s+\*\s+(.*)""" r;

  override def paragraphType: Symbol = 'Line

  override def toHtml(tokens: List[String]): String = {
    line match {
      case itemExtractor(item) => {
        Node(Some("li"), renderHits(item, tokens))
          .className("un-order-list")
          .setOuterNode(Node(Some("ul"), ""))
      }
      case _ => super.toHtml(tokens)
    }
  }
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

  override def hit(token: String): List[(Boolean, Boolean, Int, Symbol)] = {
    lines.flatMap(hitScore(_, token)) ++ hitScore(title.getOrElse(""), token)
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

case class Memo(ctype: Option[String], title: Option[String]) extends Chapter(ctype, title) {

  override def paragraphType: Symbol = 'Memo

  override def renderedBody(tokens: List[String]): String = {
    val base = new StringBuilder
    lines.foreach(code => {
      base.append(Node(Some("div"), renderHits(code, tokens)).className("comment-line"))
    })
    base.toString
  }
}

object Extractor {
  val titleYearExtractor = """(.*?)([^\\/]*?)-(\d{4})-(.*)""" r
  val dayMonthExtractor = """(\d{1,2})/(\d{1,2})""" r

  val titleExtractor = """##\s+(.*)""" r
  val subTitleExtractor = """###\s+(.*)""" r

  val globalTagsExtractor = """globalTags:\s+(.*)""" r

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