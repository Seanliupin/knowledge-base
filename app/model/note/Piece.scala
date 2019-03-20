package model.note

import model.html.Node

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 10:53 PM
  * 代表一则笔记
  */
case class Piece(title: Option[Title], fileName: Option[String]) extends Render {
  protected var lines: List[Paragraph] = List()
  private var time: Option[Time] = None

  def addLine(line: Paragraph) = {
    lines = lines ++ List(line)
  }

  def setLines(lines: List[Paragraph]) = {
    this.lines = lines
  }

  def getLines = lines

  def setTime(time: Time) = {
    this.time = Some(time)
  }

  def hasNotTime = time == None

  def isValid: Boolean = title != None

  /**
    * todo:
    * idea: 不同的命中词在搜索文本中的位置信息也可以用于计算命中分数，命中token之间相隔越近，则分数越高
    **/
  def search(tokens: List[String], context: Option[String]): Option[HitScore] = {
    val tipExtractor = """tip:([\w|\*]*)""" r;
    val codeExtractor = """code:([\w|\*]*)""" r;
    val memoExtractor = """memo:([\w|\*]*)""" r;
    val inExtractor = """in:([\w|\*]*)""" r;

    //需要将title纳入搜索范围
    val lines = title match {
      case Some(realTitle) => realTitle +: this.lines
      case _ => this.lines
    }

    val selectors = tokens.map {
      case token@tipExtractor(tipType) => (Some('Tip, tipType), token)
      case token@codeExtractor(tipType) => (Some('Code, tipType), token)
      case token@memoExtractor(tipType) => (Some('Memo, tipType), token)
      case token@inExtractor(tipType) => (Some('In, tipType), token)
      case token => (None, token)
    }

    val validSelectors = selectors.filter(it => it._1.isDefined)
    val firstSelector = if (validSelectors.isEmpty) {
      None
    } else {
      validSelectors.head._1
    }

    val leftTokens = selectors.filter(it => it._1.isEmpty).map(it => it._2)

    Score.keyWordToSymbol(context) match {
      case Some('All) => {
        if (tokens.isEmpty) return None
        val validInItems = List("memo", "url", "tip", "code", "title")

        firstSelector match {
          case Some((s, sType)) if s == 'In && validInItems.contains(sType) => {
            sType match {
              case "memo" => search(leftTokens, lines.filter(line => line.paragraphType == 'Memo), withinItem = true)
              case "code" => search(leftTokens, lines.filter(line => line.paragraphType == 'Code), withinItem = true)
              case "tip" => search(leftTokens, lines.filter(line => line.paragraphType == 'Tip), withinItem = true)
              case "url" => search(leftTokens, lines.filter(line => line.paragraphType == 'Web || line.paragraphType == 'Book), withinItem = true)
              case "title" => search(leftTokens, lines.filter(line => line.paragraphType == 'Title || line.paragraphType == 'SubTitle), withinItem = true)
              case "keywords" => search(leftTokens, lines.filter(line => line.paragraphType == 'KeyWord), withinItem = true)
              case _ => searchContent(leftTokens)
            }
          }
          case Some((s, sType)) if s == 'Tip || s == 'Code || s == 'Memo => {
            if (sType == "*") {
              search(leftTokens, lines.filter(line => line.paragraphType == s), withinItem = true)
            } else {
              search(leftTokens, lines.filter(line => line.paragraphType == s && line.constrain(sType)), withinItem = true)
            }
          }
          case _ => searchContent(tokens)
        }
      }
      case Some('Url) => {
        search(tokens, lines.filter(line => line.paragraphType == 'Web || line.paragraphType == 'Book), renderOnly = true, withinItem = true)
      }
      case Some('Title) => {
        search(tokens, lines.filter(line => line.paragraphType == 'Title || line.paragraphType == 'SubTitle), withinItem = true)
      }
      case Some('KeyWord) => {
        search(tokens, lines.filter(line => line.paragraphType == 'KeyWord))
      }
      case Some('Memo) => {
        if (tokens.isEmpty) return search(tokens, lines.filter(line => line.paragraphType == 'Memo), renderOnly = true, withinItem = true)
        firstSelector match {
          case Some((_, sType)) => {
            search(leftTokens, lines.filter(line => line.paragraphType == 'Memo && line.constrain(sType)), renderOnly = true, withinItem = true)
          }
          case _ => search(tokens, lines.filter(line => line.paragraphType == 'Memo), renderOnly = true, withinItem = true)
        }
      }
      case Some('Tip) => {
        if (tokens.isEmpty) return search(tokens, lines.filter(line => line.paragraphType == 'Tip), renderOnly = true, withinItem = true)
        firstSelector match {
          case Some((_, sType)) => {
            search(leftTokens, lines.filter(line => line.paragraphType == 'Tip && line.constrain(sType)), renderOnly = true, true)
          }
          case _ => search(tokens, lines.filter(line => line.paragraphType == 'Tip), renderOnly = true, withinItem = true)
        }
      }
      case Some('Code) => {
        if (tokens.isEmpty) return search(tokens, lines.filter(line => line.paragraphType == 'Code), renderOnly = true, withinItem = true)
        firstSelector match {
          case Some((_, sType)) => {
            search(leftTokens, lines.filter(line => line.paragraphType == 'Code && line.constrain(sType)), renderOnly = true, withinItem = true)
          }
          case _ => search(tokens, lines.filter(line => line.paragraphType == 'Code), renderOnly = true, withinItem = true)
        }
      }
      case Some(sym) => search(tokens, lines.filter(line => line.paragraphType == sym))
      case _ => None
    }
  }

  def urlRef: Option[String] = {
    title.map(innerTitle => {
      val keywords = lines.filter(_.paragraphType == 'KeyWord)
      var append = ""
      if (keywords.nonEmpty) {
        append = "-(" + keywords.mkString("/") + ")"
      }

      Node(Some("a"), s"${innerTitle.line}${append}")
        .className("category-item")
        .addProperty("href", s"#${innerTitle.hrefId.getOrElse("")}")
        .setOuterNode(Node(Some("li"), ""))
    })
  }


  private def searchContent(tokens: List[String]): Option[HitScore] = {
    var hasHit: Map[String, Boolean] = tokens.map(token => (token, false)).toMap

    var titleScore = 0
    title match {
      case Some(realTitle) => {
        tokens.foreach(token => {
          val hitList = realTitle.hit(token)
          if (hitList.nonEmpty) {
            titleScore = Algorithm.computeScore(hitList.map(x => (x._1, x._2, x._3)), 'Title)
            hasHit = hasHit.updated(token, true)
          }
        })
      }
      case _ =>
    }

    time match {
      case Some(realTime) => {
        tokens.foreach(token => {
          val hitList = realTime.hit(token)
          if (hitList.nonEmpty) {
            titleScore = Algorithm.computeScore(hitList.map(x => (x._1, x._2, x._3)), 'Time)
            hasHit = hasHit.updated(token, true)
          }
        })
      }
      case _ =>
    }


    var allList: List[(Boolean, Boolean, Int, Symbol)] = List()

    for (token <- tokens; line <- lines; if !line.isEmpty) {
      val hitList = line.hit(token)
      if (hitList.nonEmpty) {
        hasHit = hasHit.updated(token, true)
        allList = allList ++ hitList
      }
    }

    val bodyScore = allList.groupBy(_._4).map {
      case (sym, list) => Algorithm.computeScore(list.map(x => (x._1, x._2, x._3)), sym)
    }.sum

    if (hasHit.toList.forall(_._2)) {
      Some(HitScore(renderHtml(tokens), titleScore + bodyScore))
    } else {
      None
    }
  }

  private def search(tokens: List[String], items: List[Paragraph], renderOnly: Boolean = false, withinItem: Boolean = false): Option[HitScore] = {
    var hasHit: Map[String, Boolean] = tokens.map(token => (token, false)).toMap

    /**
      * item 有可能被某些条件过滤掉了
      **/
    if (items.isEmpty) {
      return None
    }

    /**
      * 如果没有token，并且还有item，则将item返回
      **/

    if (tokens.isEmpty) {
      if (renderOnly) {
        return Some(HitScore(renderHtml(tokens, items), 10))
      } else {
        return Some(HitScore(renderHtml(tokens), 10))
      }
    }


    var allList: List[(Boolean, Boolean, Int, Symbol)] = List()
    var hitItems: List[Paragraph] = List()

    //如果仅仅在元素(url,memo,code)内搜索，则token需要在元素内全出现
    if (withinItem) {
      for (line <- items; if !line.isEmpty) {
        val allTempHits = tokens.map(t => (t, line.hit(t)))
        val fullMatch = allTempHits.forall(t => t._2.nonEmpty)
        if (fullMatch) {
          allTempHits.foreach(t => {
            hasHit = hasHit.updated(t._1, true)
            allList = allList ++ t._2
            hitItems = line +: hitItems
          })
        }
      }
    } else {
      for (token <- tokens; line <- items; if !line.isEmpty) {
        val hitList = line.hit(token)
        if (hitList.nonEmpty) {
          hasHit = hasHit.updated(token, true)
          allList = allList ++ hitList
          hitItems = line +: hitItems
        }
      }
    }


    val bodyScore = allList.groupBy(_._4).map {
      case (sym, list) => Algorithm.computeScore(list.map(x => (x._1, x._2, x._3)), sym)
    }.sum

    if (hasHit.toList.forall(_._2)) {
      if (renderOnly) {
        Some(HitScore(renderHtml(tokens, hitItems), bodyScore))
      } else {
        Some(HitScore(renderHtml(tokens), bodyScore))
      }
    } else {
      None
    }
  }

  private def renderHtml(tokens: List[String], item: List[Render]): String = {
    val html = new StringBuilder
    item.distinct.foreach(line => {
      html.append(line.toHtml(tokens))
    })
    html.toString
  }

  private def renderHtml(tokens: List[String]): String = {
    val html = new StringBuilder

    title match {
      case Some(realTitle) => {
        //title 是可以直接看到的，fileName是鼠标悬停的时候显示
        html.append(Node(Some("div"), "" + realTitle.toHtml(tokens, fileName.getOrElse("")) + time.getOrElse(Time("")).toHtml(tokens)).className("piece-title-box"))
      }
      case None => return "This is not a valid piece"
    }

    val keywords = lines.filter(_.paragraphType == 'KeyWord)
    val other = lines.filter(_.paragraphType != 'KeyWord)

    if (keywords.nonEmpty) {
      html.append(Node(Some("div"), keywords.map(_.toHtml(List())).mkString("  "))
        .className("piece-keyword-container"))
    }

    other.foreach(line => {
      html.append(line.toHtml(tokens))
    })

    Node(Some("div"), html.toString).className("piece-container").toString()
  }

  override def toHtml(tokens: List[String]): String = {
    renderHtml(tokens)
  }

  override def toPlain: String = {
    ""
  }
}
