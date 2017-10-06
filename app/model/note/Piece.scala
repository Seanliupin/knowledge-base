package model.note

import model.html.Node

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 10:53 PM
  * 代表一则笔记
  */
case class Piece(title: Option[Title], fileName: Option[String]) extends KnowledgeBase with Render {
  protected var lines: List[Paragraph] = List()
  private var time: Option[Time] = None

  def addLine(line: Paragraph) = {
    lines = lines ++ List(line)
  }

  def setTime(time: Time) = {
    this.time = Some(time)
  }

  def isValid: Boolean = title != None

  private def searchContent(tokens: List[String]): List[HitScore] = {
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

    lines.groupBy(_.paragraphType).foreach(y => {
      val zipItem = tokens zip y._2
      zipItem.map(x => (x._1, x._2.hit(x._1))) //x._1 is token, x._2 is Hit
        .filter(_._2 > 0) // filter out hitScore == 0
        .foreach(x => {
        val score = scores.getOrElse(x._1, 0)
        scores = scores.updated(x._1, score + x._2)
      })
    })

    val contain = scores.toList.forall(_._2 > 0)
    val totalScore = scores.toList.map(_._2).sum

    if (contain) {
      List(HitScore(renderHtml(tokens), totalScore))
    } else {
      List(HitScore("", 0))
    }
  }

  private def search(tokens: List[String], items: List[Hit]): List[HitScore] = {
    var scores: Map[String, Int] = tokens.map(token => (token, 0)).toMap

    val zipItem = tokens zip items
    zipItem.map(x => (x._1, x._2.hit(x._1))) //x._1 is token, x._2 is Hit
      .filter(_._2 > 0) // filter out hitScore == 0
      .foreach(x => {
      val score = scores.getOrElse(x._1, 0)
      scores = scores.updated(x._1, score + x._2)
    })

    val contain = scores.toList.forall(_._2 > 0)
    val totalScore = scores.toList.map(_._2).sum

    if (contain) {
      List(HitScore(renderHtml(tokens), totalScore))
    } else {
      List(HitScore("", 0))
    }
  }

  override def search(tokens: List[String], context: Option[String]): List[HitScore] = {
    Score.keyWordToSymbol(context) match {
      case Some('All) => searchContent(tokens)
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
        val extractor = """tip:(.*)""" r;
        tokens.foreach {
          case token@extractor(tipType) => {
            return search(tokens.filter(_ != token), lines.filter(line => line.paragraphType == 'Tip && line.constrain(tipType)))
          }
          case _ =>
        }
        search(tokens, lines.filter(line => line.paragraphType == 'Tip))
      }
      case Some('Code) => {
        val extractor = """code:(.*)""" r;
        tokens.foreach {
          case token@extractor(lan) => {
            return search(tokens.filter(_ != token), lines.filter(line => line.paragraphType == 'Code && line.constrain(lan)))
          }
          case _ =>
        }
        search(tokens, lines.filter(line => line.paragraphType == 'Code))
      }
      case Some(sym) => search(tokens, lines.filter(line => line.paragraphType == sym))
      case _ => List()
    }
  }

  protected def renderHtml(tokens: List[String]): String = {
    val html = new StringBuilder

    title match {
      case Some(realTitle) => {
        //title 是可以直接看到的，fileName是鼠标悬停的时候显示
        html.append(Node("div", "" + realTitle.toHtml(tokens, fileName.getOrElse("")) + time.getOrElse(Time("")).toHtml(tokens)).className("piece-title-box"))
      }
      case None => return "This is not a valid piece"
    }

    val keywords = lines.filter(_.paragraphType == 'KeyWord)
    val other = lines.filter(_.paragraphType != 'KeyWord)

    if (keywords.size > 0) {
      html.append(Node("div", keywords.map(_.toHtml(List())).mkString("  "))
        .className("piece-keyword-container"))
    }

    other.foreach(line => {
      html.append(line.toHtml(tokens))
    })


    Node("div", html.toString).className("piece-container").toString()
  }

  override def toHtml(tokens: List[String]): String = {
    renderHtml(tokens)
  }

  override def toPlain: String = {
    ""
  }
}
