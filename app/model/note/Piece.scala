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
  protected var keywords: List[KeyWord] = List()
  private var time: Option[Time] = None

  def addLine(line: Paragraph) = {
    lines = lines ++ List(line)
  }

  def addKeyword(newKeyword: KeyWord) = {
    keywords = newKeyword +: keywords
  }

  def setTime(time: Time) = {
    this.time = Some(time)
  }

  def isValid: Boolean = title != None

  def searchContent(tokens: List[String]): List[HitScore] = {
    var scores: Map[String, Int] = tokens.map(token => (token, 0)).toMap

    title match {
      case Some(realTitle) => {
        tokens.filter(token => realTitle.hit(token))
          .foreach(token => {
            val score = scores.getOrElse(token, 0)
            scores = scores.updated(token, score + Score.scoreTitle)
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

    tokens.filter(token => keywords.exists(_.hit(token)))
      .foreach(token => {
        val score = scores.getOrElse(token, 0)
        scores = scores.updated(token, score + Score.scoreTag)
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
    context match {
      case Some("keyword") => search(tokens, keywords, Score.scoreTag)
      case Some("comment") => search(tokens, lines.filter(line => line.paragraphType == 'Comment), Score.scoreComment)
      case Some("web") => search(tokens, lines.filter(line => line.paragraphType == 'Web), Score.scoreComment)
      case Some("book") => search(tokens, lines.filter(line => line.paragraphType == 'Book), Score.scoreComment)
      case Some("code") => search(tokens, lines.filter(line => line.paragraphType == 'Code), Score.scoreBody)
      case Some("body") => search(tokens, lines.filter(line => line.paragraphType == 'Line), Score.scoreBody)
      case Some("all") => searchContent(tokens)
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

    if (keywords.size > 0) {
      html.append(Node("div", keywords.map(keyword => {
        Node("code", keyword.toString).className("piece-keyword").toString()
      }).mkString(" ")).className("piece-keyword-container"))
    }

    lines.foreach(line => {
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
