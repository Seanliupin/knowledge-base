package model.note

import helper.StringUtil
import model.html.HtmlNode

/**
  * Author: Sean
  * Date: 5/10/2017
  * Time: 10:33 AM
  */
trait Hit {
  /**
    * 返回命中结果
    **/
  def hit(token: String): List[(Boolean, Boolean, Int, Symbol)]
}

trait Render {
  /**
    * 将该元素渲染成html节点元素。
    **/
  def toHtml(tokens: List[String]): String

  /**
    * 该函数主要是将搜索关键词高亮显示出来，不同的元素可以有不同的高亮方案。
    *
    * 需要把url单独出来，否则经渲染后url格式会被破坏掉，从而在网页上不能点击
    *
    * 因为代码段中，可能会有/* comment */ 的注释块。此时，需要用renderStrong关闭对强调的渲染
    **/
  protected def renderHits(text: String, tokens: List[String], renderStrong: Boolean = true): String = {
    if (text.trim.isEmpty || tokens.isEmpty) return text
    //所有形如<tx attribute>yyy</tx>的html元素都只渲染其中yyy部分，同时还要保留tx和其attribute值
    val urlPattern = """(.*?)\<[\s]*(\w+?)(.*)\>(.*?)\<\/[\s]*(\2)[\s]*\>(.*)""" r;
    val strongPattern = """(.*?)([`*_]+)(.+?)(\2)(.*)""" r;
    text match {
      case urlPattern(head, marker, attribute, body, _, tail) => {
        renderHits(head, tokens) + s"<$marker $attribute>" + renderHits(body, tokens) + s"</$marker>" + renderHits(tail, tokens)
      }
      case strongPattern(head, marker, strong, _, tail) if renderStrong => {
        val tc = marker match {
          case "*" => "one"
          case "`" => "two"
          case _ => "normal"
        }

        val strongStr = HtmlNode(Some("strong"), renderHits(strong, tokens)).className(s"text-strong-$tc").toString()
        renderHits(head, tokens) + strongStr + renderHits(tail, tokens)
      }
      case _ => renderHitsWithPlainStr(text, tokens)
    }
  }

  /**
    * 渲染不带url的文本
    **/
  private def renderHitsWithPlainStr(text: String, tokens: List[String]): String = {
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
        HtmlNode(Some("strong"), sub).className("searched-text").toString()
      } else {
        sub
      }
    }).mkString("")
  }

  /**
    * render hit with strong by default, sub class can render hit their style
    **/
  private def renderHit(text: String, token: String, offset: Int): List[(Int, Int)] = {
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
    List(title, href, comment).flatMap(item => hitScore(item, token, paragraphType))
  }

  override def toHtml(tokens: List[String]): String = {
    val url = HtmlNode(Some("a"), renderHits(title, tokens))
      .href(href)
      .className(linkClassName)
      .addProperty("target", "_blank")
    var comm = ""
    if (StringUtil.isNotBlank(comment)) {
      comm = HtmlNode(Some("div"), renderHits(comment, tokens)).className("piece-url-comment")
    }
    HtmlNode(Some("div"), url + comm).className("piece-url-container")
  }
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
    hitScore(line, token, paragraphType)
  }

  def isEmpty: Boolean = line.trim.length == 0

  /**
    * 计算命中分数，全字符命中得分最高
    * todo: 是否可以不用正则表达式来实现该算法
    * paragraphType通常是本paragraph的type。加入该参数，是为了可以更好地控制分值的计算。
    * 比如，一则memo有title和正文。则搜索memo的时候，可以将title和正文分开计算分值
    **/
  final protected def hitScore(text: String, token: String, paragraphType: Symbol): List[(Boolean, Boolean, Int, Symbol)] = {
    getMatchList(token, text).map(x => (x._1, x._2, x._3, paragraphType))
  }

  /**
    * List[(是否全字符匹配, 是否大小写匹配，单词位置)]
    **/
  private def getMatchList(word: String, target: String, offset: Int = 0): List[(Boolean, Boolean, Int)] = {
    if (target.trim.isEmpty) return List()
    val letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val ignoreCase = word.map(c => {
      if (letters.contains(c)) {
        s"[${c.toLower}|${c.toUpper}]"
      } else if ("\\/|()[]{}+.*?".contains(c)) {
        s"\\$c"
      } else {
        c
      }
    }).mkString("")

    val ignoreCaseNotWhole = s"(.*?)(${ignoreCase})(.*)".r
    val ignoreCaseWhole = s"(.*?)\\b(${ignoreCase})\\b(.*)".r

    target match {
      case ignoreCaseWhole(head, matched, tail) => {
        (true, matched == word, offset + head.length) +: getMatchList(word, tail, offset + head.length + matched.length)
      }
      case ignoreCaseNotWhole(head, matched, tail) => {
        (false, matched == word, offset + head.length) +: getMatchList(word, tail, offset + head.length + matched.length)
      }
      case _ => List()
    }
  }

  final protected def hitWord: Int = weight * 5

  final protected def hitWordIgnoreCase: Int = weight * 3

  final protected def hitInWord: Int = weight * 2

  final protected def hitInWordIgnoreCase: Int = weight

  protected def weight: Int = Score.getScore(paragraphType)

  override def toString: String = line

  def paragraphType: Symbol

  def constrain(only: String): Boolean = true

  override def toHtml(tokens: List[String]): String = {
    val node = HtmlNode(Some("p"), renderHits(line, tokens)).className("piece-content")
    node.toString()
  }
}

case class Title(title: Option[String], hrefId: Option[String] = None) extends Paragraph(title.getOrElse("")) {

  def toHtml(tokens: List[String], fileName: String): String = {
    if (title.isEmpty) return ""

    val titleNode = HtmlNode(Some("a"), renderHits(title.get, tokens)).className("piece-title").title(fileName)
    hrefId match {
      case Some(href) => HtmlNode(Some("div"), titleNode).addProperty("id", href)
      case None => titleNode
    }
  }

  override def paragraphType: Symbol = 'Title
}

/**
  * 新增FileName元素，使得笔记所在的文件名也可以被搜索。这是一个隐形元素，不参与渲染，但参与搜索
  **/
case class FileName(name: String, hrefId: Option[String] = None) extends Paragraph(name) {
  override def paragraphType: Symbol = 'FileName

  override def toHtml(tokens: List[String]): String = ""
}

case class SubTitle(line: String) extends Paragraph(line) {
  override def toHtml(tokens: List[String]): String = HtmlNode(Some("p"), renderHits(line, tokens)).className("piece-h3")

  override def paragraphType: Symbol = 'SubTitle
}

case class Tag(line: String) extends Paragraph(line) {
  override def paragraphType: Symbol = 'Tag

  override def toHtml(tokens: List[String]): String = HtmlNode(Some("code"), line).className("piece-keyword").toString()
}

/**
  * 对于script来讲，src没有太多意义，只有对它的描述有点意义。
  **/
case class Frame(attribute: String, des: String, srcOp: Option[String] = None) extends Paragraph(des) {

  override def paragraphType: Symbol = 'Frame

  /**
    * 其始终都不应该是空
    **/
  override def isEmpty: Boolean = false

  /**
    * 让网址参与搜索，使得可以通过网址过滤frame
    **/
  override def hit(token: String): List[(Boolean, Boolean, Int, Symbol)] = {
    srcOp match {
      case Some(src) => List(des, src).flatMap(item => hitScore(item, token, paragraphType))
      case _ => hitScore(des, token, paragraphType)
    }
  }

  override def toHtml(tokens: List[String]): String = {
    val content = if (des.trim().isEmpty) {
      ""
    } else {
      HtmlNode(Some("div"), renderHits(des, tokens)).className("frame-title").toString()
    }
    HtmlNode(Some("div"), content + s"<iframe $attribute></iframe> ").className("frame-body").toString()
  }

}

case class Time(line: String) extends Paragraph(line) {
  override def toHtml(tokens: List[String]): String = HtmlNode(Some("text"), line).className("piece-time")

  override def paragraphType: Symbol = 'Time
}

case class Id(line: String) extends Paragraph(line) {
  /**
    * 不渲染ID，其只用来查找
    **/
  override def toHtml(tokens: List[String]): String = ""

  override def paragraphType: Symbol = 'Id
}

case class Line(line: String) extends Paragraph(line) {
  private val itemExtractor = """\s+\*\s+(.*)""" r;
  private val sharpExtractor = """\s*\#\s+(.*)""" r;

  override def paragraphType: Symbol = 'Line

  override def toHtml(tokens: List[String]): String = {

    line match {
      case itemExtractor(item) => {
        HtmlNode(Some("li"), renderHits(item, tokens))
          .className("un-order-list")
          .setOuterNode(HtmlNode(Some("ul"), ""))
      }
      case sharpExtractor(_) => {
        val node = HtmlNode(Some("p"), renderHits(line, tokens))
          .className("piece-content")
          .className("sharp-line")
        node.toString()
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

  override def constrain(only: String): Boolean = tipType.contains(only)

  override def toHtml(tokens: List[String]): String = HtmlNode(Some("p"), renderHits(line, tokens)).className("piece-tip").className(colorClass)
}

/**
  * 多行组成的一个块，比如一段代码，一段评注等。用某一个
  **/
abstract class Chapter(cType: Option[String], titleOp: Option[String]) extends Paragraph(titleOp.getOrElse("")) {
  protected var lines: List[String] = List()

  def isValid: Boolean = cType.isDefined

  protected def titleType: Symbol

  def addLine(line: String) = {
    lines = lines ++ List(line)
  }

  override def hit(token: String): List[(Boolean, Boolean, Int, Symbol)] = {
    titleOp match {
      case Some(title) => lines.flatMap(hitScore(_, token, paragraphType)) ++ hitScore(title, token, titleType)
      case _ => lines.flatMap(hitScore(_, token, paragraphType))
    }
  }

  override def constrain(only: String): Boolean = cType.contains(only)

  override def isEmpty: Boolean = {
    titleOp.getOrElse("").length == 0 && lines.forall(_.trim.length == 0)
  }

  // memo or code
  final def chapterType: String = paragraphType.toString().toLowerCase.filter((i: Char) => i != '\'')

}

/**
  * 将代码按行原样输出，浏览器里有插件自动对其渲染。
  **/
case class Code(lanOp: Option[String], titleOp: Option[String]) extends Chapter(lanOp, titleOp) {
  override def paragraphType: Symbol = 'Code

  protected def titleType: Symbol = 'CodeTitle

  override def toHtml(tokens: List[String]): String = {
    val block = lines.map(line => {
      lanOp match {
        case Some(lan) if lan == "xml" || lan == "html" => {
          line.replace("<", "&lt;").replace(">", "&gt;")
        }
        case _ => line
      }
    }).map(line => {
      renderHits(line, tokens, renderStrong = false)
    }).mkString("\n")

    val lanStr = lanOp.getOrElse("none")

    val codeBlock = HtmlNode(Some("code"), block)
      .className(lanStr)
      .toString()

    val bodyNode = HtmlNode(Some("pre"), codeBlock).toString()

    val titleNode = titleOp match {
      case Some(title) if title.trim.nonEmpty => {
        val metaNode = lanOp match {
          case Some(lan) if lan.nonEmpty => "" + HtmlNode(Some("div"), s"${lan.capitalize}: ")
            .className("block-meta")
            .className(s"$chapterType-meta")
            .className(s"$lan-meta")
          case _ => ""
        }

        HtmlNode(Some("div"), metaNode + renderHits(title, tokens))
          .className(s"$chapterType-title")
          .className(s"code-title-$lanStr")
          .toString()
      }
      case _ => ""
    }


    HtmlNode(Some("div"), titleNode + bodyNode)
  }
}

case class Memo(cType: Option[String], titleOp: Option[String]) extends Chapter(cType, titleOp) {
  override def paragraphType: Symbol = 'Memo

  protected def titleType: Symbol = 'MemoTitle

  private def renderedBody(tokens: List[String], divClass: List[String]): String = {
    val subNotes = NoteFile("").linesToNotes(lines, "", None, None, Some(Title(None, None)))
    val body = subNotes.reverse.map(_.toHtml(tokens)).mkString("")
    HtmlNode(Some("div"), body)
      .classNames(divClass)
      .className(s"$chapterType-body").toString()
  }

  override def toHtml(tokens: List[String]): String = {
    val typeName = cType match {
      case Some(t) => s"$chapterType-$t"
      case None => s"$chapterType-blank"
    }

    var titleNode = ""
    titleOp match {
      case Some(title) => {
        val metaNode = cType match {
          case Some(subType) if subType.nonEmpty => "" + HtmlNode(Some("div"), s"${subType.capitalize}: ")
            .className("block-meta")
            .className(s"$chapterType-meta")
            .className(s"$subType-meta")
          case _ => ""
        }

        if (title.trim.nonEmpty) {
          titleNode = HtmlNode(Some("div"), metaNode + renderHits(title, tokens))
            .className(s"$chapterType-title")
            .className(s"$typeName-title")
        }
      }
      case None =>
    }

    HtmlNode(Some("div"), titleNode + renderedBody(tokens, List(s"$chapterType-block", s"$typeName-block"))).className(s"$chapterType")

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
  val idExtractor = """id:\s+(.*)""" r

  val frameExtractor = """\<iframe\s+(.*?)\>(.*?)\<\/iframe\>""" r
  val srcExtractor = """(.*?)src=\"(.*?)\"(.*)""" r

  val typeLessTipExtractor = """>(.*)""" r
  val typedTipExtractor = """>(\w+?):\s*(.*)""" r

  val codeHeaderExtractor = """```(\w*)[:]?\b*(.*)""" r
  val memoHeaderExtractor = """'''(\w*)[:]?\b*(.*)""" r

  val codeFooterExtractor = """```""" r
  val memoFooterExtractor = """'''""" r
}