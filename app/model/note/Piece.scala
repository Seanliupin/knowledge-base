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
    val tipExtractor = """tip:(.*)""" r;
    val codeExtractor = """code:(.*)""" r;

    //需要将title纳入搜索范围
    val lines = title match {
      case Some(realTitle) => realTitle +: this.lines
      case _ => this.lines
    }

    Score.keyWordToSymbol(context) match {
      case Some('All) => {
        if (tokens.size == 0) return None

        val last = tokens.last
        last match {
          case tipExtractor(tipType) => {
            search(tokens.filter(_ != last), lines.filter(line => line.paragraphType == 'Tip && line.constrain(tipType)))
          }
          case codeExtractor(lan) => {
            search(tokens.filter(_ != last), lines.filter(line => line.paragraphType == 'Code && line.constrain(lan)))
          }
          case _ => searchContent(tokens)
        }

      }
      case Some('Url) => {
        search(tokens, lines.filter(line => line.paragraphType == 'Web || line.paragraphType == 'Book))
      }
      case Some('Title) => {
        search(tokens, lines.filter(line => line.paragraphType == 'Title || line.paragraphType == 'SubTitle))
      }
      case Some('Comment) => {
        search(tokens, lines.filter(line => line.paragraphType == 'Tip && line.constrain("note")))
      }
      case Some('Tip) => {
        if (tokens.size == 0) return search(tokens, lines.filter(line => line.paragraphType == 'Tip), true)
        val last = tokens.last
        last match {
          case tipExtractor(tipType) => {
            search(tokens.filter(_ != last), lines.filter(line => line.paragraphType == 'Tip && line.constrain(tipType)), true)
          }
          case _ => search(tokens, lines.filter(line => line.paragraphType == 'Tip), true)
        }
      }
      case Some('Code) => {
        if (tokens.size == 0) return search(tokens, lines.filter(line => line.paragraphType == 'Code), true)
        val last = tokens.last
        last match {
          case codeExtractor(lan) => {
            search(tokens.filter(_ != last), lines.filter(line => line.paragraphType == 'Code && line.constrain(lan)), true)
          }
          case _ => search(tokens, lines.filter(line => line.paragraphType == 'Code), true)
        }


      }
      case Some(sym) => search(tokens, lines.filter(line => line.paragraphType == sym))
      case _ => None
    }
  }

  private def searchContent(tokens: List[String]): Option[HitScore] = {
    var scores: Map[String, Int] = tokens.map(token => (token, 0)).toMap

    title match {
      case Some(realTitle) => {
        tokens.foreach(token => {
          val hitScore = realTitle.hit(token)
          if (hitScore > 0) {
            val score = scores.getOrElse(token, 0)
            scores = scores.updated(token, score + hitScore)
          }
        })
      }
      case _ =>
    }

    for (token <- tokens; line <- lines; if !line.isEmpty) {
      val hitScore = line.hit(token)
      if (hitScore > 0) {
        val score = scores.getOrElse(token, 0)
        scores = scores.updated(token, score + hitScore)
      }
    }

    val contain = scores.toList.forall(_._2 > 0)
    val totalScore = scores.toList.map(_._2).sum

    if (contain) {
      Some(HitScore(renderHtml(tokens), totalScore))
    } else {
      None
    }
  }

  private def search(tokens: List[String], items: List[Paragraph], renderOnly: Boolean = false): Option[HitScore] = {
    var scores: Map[String, Int] = tokens.map(token => (token, 0)).toMap

    /**
      * item 有可能被某些条件过滤掉了
      **/
    if (items.size == 0) {
      return None
    }

    /**
      * 如果没有token，并且还有item，则将item返回
      **/

    if (tokens.size == 0) {
      if (renderOnly) {
        return Some(HitScore(renderHtml(tokens, items), 10))
      } else {
        return Some(HitScore(renderHtml(tokens), 10))
      }
    }

    var hitItems: List[Paragraph] = List()

    for (token <- tokens; line <- items; if !line.isEmpty) {
      val hitScore = line.hit(token)
      if (hitScore > 0) {
        val score = scores.getOrElse(token, 0)
        scores = scores.updated(token, score + hitScore)

        hitItems = line +: hitItems
      }
    }

    val contain = scores.toList.forall(_._2 > 0)
    val totalScore = scores.toList.map(_._2).sum

    if (contain) {
      if (renderOnly) {
        return Some(HitScore(renderHtml(tokens, hitItems), totalScore)) // 这里的hitItems不用reverse，因为在最外层，其会根据score重新排序
      } else {
        return Some(HitScore(renderHtml(tokens), totalScore))
      }
    } else {
      None
    }
  }

  private def renderHtml(tokens: List[String], item: List[Render]): String = {
    val html = new StringBuilder
    item.foreach(line => {
      html.append(line.toHtml(tokens))
    })
    html.toString
  }

  protected def renderHtml(tokens: List[String]): String = {
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

    if (keywords.size > 0) {
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
