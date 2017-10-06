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
        tokens.filter(token => realTitle.hit(token))
          .foreach(token => {
            val score = scores.getOrElse(token, 0)
            scores = scores.updated(token, score + Score.getScore('Title))
          })
      }
      case _ =>
    }

    lines.groupBy(_.paragraphType).foreach(x => {
      tokens.filter(token => x._2.exists(_.hit(token)))
        .foreach(token => {
          val score = scores.getOrElse(token, 0)
          scores = scores.updated(token, score + Score.getScore(x._1))
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

  private def search(tokens: List[String], items: List[Hit], scoreValue: Int): List[HitScore] = {
    var scores: Map[String, Int] = tokens.map(token => (token, 0)).toMap

    tokens.filter(token => items.exists(_.hit(token)))
      .foreach(token => {
        val score = scores.getOrElse(token, 0)
        scores = scores.updated(token, score + scoreValue)
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
        search(tokens, lines.filter(line => line.paragraphType == 'Web || line.paragraphType == 'Book), Score.getScore('Comment))
      }
      case Some('Title) => {
        search(tokens, lines.filter(line => line.paragraphType == 'Title || line.paragraphType == 'SubTitle), Score.getScore('Comment))
      }
      case Some('Comment) => {
        search(tokens, lines.filter(line => line.paragraphType == 'Tip && line.constrain("note")), Score.getScore('Comment))
      }
      case Some('Tip) => {
        val extractor = """tip:(.*)""" r;
        tokens.foreach {
          case token@extractor(tipType) => {
            return search(tokens.filter(_ != token), lines.filter(line => line.paragraphType == 'Tip && line.constrain(tipType)), Score.getScore('Tip))
          }
          case _ =>
        }
        search(tokens, lines.filter(line => line.paragraphType == 'Tip), Score.getScore('Tip))
      }
      case Some('Code) => {
        val extractor = """code:(.*)""" r;
        tokens.foreach {
          case token@extractor(lan) => {
            return search(tokens.filter(_ != token), lines.filter(line => line.paragraphType == 'Code && line.constrain(lan)), Score.getScore('Code))
          }
          case _ =>
        }
        search(tokens, lines.filter(line => line.paragraphType == 'Code), Score.getScore('Code))
      }
      case Some(sym) => search(tokens, lines.filter(line => line.paragraphType == sym), Score.getScore(sym))
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
