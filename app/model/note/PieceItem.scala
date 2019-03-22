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

  final def renderHits(tokens: List[String]): String = {
    renderHits(toPlain, tokens)
  }

  /**
    * 需要把url单独出来，否则经渲染后url格式会被破坏掉，从而在网页上不能点击
    **/
  protected def renderHits(text: String, tokens: List[String]): String = {
    if (text.trim.isEmpty) return text

    val pattern = """(.*?)\<a(.*?)\>(.*)</a>(.*)""" r;
    text match {
      case pattern(head, href, body, tail) => {
        renderHitsNoUrl(head, tokens) + s"<a $href>" + renderHitsNoUrl(body, tokens) + "</a>" + renderHits(tail, tokens)
      }
      case _ => renderHitsNoUrl(text, tokens)
    }
  }

  /**
    * 渲染不带url的文本
    **/
  private def renderHitsNoUrl(text: String, tokens: List[String]): String = {
    var span = List[(Int, Int)]()
    tokens.foreach(token => {
      span = renderHit(text, token, 0) ++ span
    })

    val coloredSpan = span.filter(s => s._2 > s._1)
      .sortBy(_._2)
      .foldRight(List[(Int, Int, Boolean)]((text.length, text.length, true))) {
        (item, coll) => {
          if (item._2 >= coll.head._1) {
            (item._1, coll.head._2, true) +: coll.tail
          } else {
            List((item._1, item._2, true), (item._2, coll.head._1, false)) ++ coll
          }
        }
      }

    val fullColoredSpan = (0, coloredSpan.head._1, false) +: coloredSpan
    fullColoredSpan.filter(s => !(s._1 >= text.length || s._2 <= 0)).map(it => {
      val sub = text.substring(it._1, it._2)
      if (it._3) {
        Node(Some("strong"), sub).className("text-danger").toString()
      } else {
        sub
      }
    }).mkString("")
  }

  /**
    * render hit with strong by default, sub class can render hit their style
    **/
  protected def renderHit(text: String, token: String, offset: Int): List[(Int, Int)] = {
    val _token = token.map(c => {
      if (c.isLetter) {
        s"[${c.toUpper}|${c.toLower}]"
      } else if ("()[]{}+.*?".contains(c)) {
        s"\\$c"
      } else {
        c
      }
    }).mkString("")

    val extractor = s"(.*?)(${_token})(.*)" r

    text match {
      case extractor(head, _, tail) => {
        (offset + head.length, offset + text.length - tail.length) +: renderHit(tail, token, offset + text.length - tail.length)
      }
      case _ => List()
    }
  }
}


abstract class Link(title: String, href: String, comment: String) extends Paragraph(title) {
  protected def linkClassName: String = "piece-url"

  override def hit(token: String): List[(Boolean, Boolean, Int, Symbol)] = {
    List(title, href, comment).flatMap(item => hitScore(item, token))
  }

  override def toHtml(tokens: List[String]): String = {
    val url = Node(Some("a"), renderHits(title, tokens))
      .href(href)
      .className(linkClassName)
      .addProperty("target", "_blank")
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
        s"[${c.toLower}|${c.toUpper}]"
      } else if ("()[]{}+.*?".contains(c)) {
        s"\\$c"
      } else {
        c
      }
    }).mkString("")

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
    val titleNode = Node(Some("a"), renderHits(line, tokens)).className("piece-title").title(fileName)
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

/**
  * 对于script来讲，src没有太多意义，只有对它的描述有点意义。
  **/
case class Script(src: String, des: String) extends Paragraph(des) {

  override def paragraphType: Symbol = 'Script

  /**
    * 其始终都不应该是空
    **/
  override def isEmpty: Boolean = false


  override def hit(token: String): List[(Boolean, Boolean, Int, Symbol)] = {
    hitScore(des, token)
  }

  override def toHtml(tokens: List[String]): String = {
    val scriptDes = if (des.trim().isEmpty) {
      ""
    } else {
      Node(Some("div"), renderHits(des, tokens)).className("script-des").toString()
    }
    Node(Some("div"), scriptDes + s"<script src=$src></script> ").className("script-body").toString()
  }

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
abstract class Chapter(cType: Option[String], title: Option[String]) extends Paragraph(title.getOrElse("")) {
  protected var lines: List[String] = List()

  def isValid: Boolean = cType != None

  def addLine(line: String) = {
    lines = lines ++ List(line)
  }

  override def hit(token: String): List[(Boolean, Boolean, Int, Symbol)] = {
    lines.flatMap(hitScore(_, token)) ++ hitScore(title.getOrElse(""), token)
  }

  override def constrain(only: String): Boolean = cType == Some(only)

  override def isEmpty: Boolean = {
    title.getOrElse("").length == 0 && lines.forall(_.trim.length == 0)
  }

  // memo or code
  final def chapterType: String = paragraphType.toString().toLowerCase.filter((i: Char) => i != '\'')

  /**
    * 子类可以自己渲染其内容，比如代码段可以自行根据语言类型进行渲染
    **/
  protected def renderedBody(tokens: List[String]): String = {
    val base = new StringBuilder
    lines.foreach(code => {
      base.append(Node(Some("div"), renderHits(code, tokens)).className(s"$chapterType-line"))
    })

    base.toString()
  }

  override def toHtml(tokens: List[String]): String = {
    val typeName = cType match {
      case Some(t) => s"$chapterType-$t"
      case None => s"$chapterType-blank"
    }

    var titleNode = ""
    title match {
      case Some(t) => {
        val metaNode = cType match {
          case Some(subType) if subType.nonEmpty => "" + Node(Some("div"), s"${subType.capitalize}: ")
            .className("block-meta")
            .className(s"$chapterType-meta")
            .className(s"$subType-meta")
          case _ => ""
        }

        if (t.trim.length > 0) {
          titleNode = Node(Some("div"), metaNode + t)
            .className(s"$chapterType-title")
            .className(s"$typeName-title")
        }
      }
      case None =>
    }

    val bodyNode = Node(Some("div"), renderedBody(tokens))
      .className(s"$chapterType-block")
      .className(s"$typeName-block")

    Node(Some("div"), titleNode + bodyNode).className(s"$chapterType")
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

  val scriptExtractor = """\<script\s+src=(.*?)\>(.*?)\<\/script\>""" r

  val typeLessTipExtractor = """>(.*)""" r
  val typedTipExtractor = """>(.*?):\s*(.*)""" r

  val codeHeaderExtractor = """```(\w*)[:]?\b*(.*)""" r
  val commentHeaderExtractor = """'''(\w*)[:]?\b*(.*)""" r

  val codeFooterExtractor = """```""" r
  val commentFooterExtractor = """'''""" r
}