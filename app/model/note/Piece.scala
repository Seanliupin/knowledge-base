package model.note

import model.html.Node

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 10:53 PM
  * 代表一则笔记
  */
case class Piece(title: String, fileName: Option[String]) extends Searchable {
  protected var lines: List[String] = List()
  protected var keywords: List[String] = List()
  protected var comments: List[String] = List()
  protected var urls: List[Url] = List()

  def addLine(line: String) = {
    lines = lines ++ List(line)
  }

  def addKeywords(newKeywords: List[String]) = {
    keywords = keywords ++ newKeywords
  }

  def addComment(comment: String) = {
    comments = comments ++ List(comment)
  }

  def isNotEmpty: Boolean = title.trim.length > 0

  def searchContent(tokens: List[String]): List[Hit] = {
    var scores: Map[String, Int] = tokens.map(token => (token, 0)).toMap

    tokens.filter(token => title.contains(token))
      .foreach(token => {
        val score = scores.getOrElse(token, 0)
        scores = scores.updated(token, score + Score.scoreTitle)
      })

    val totalLines = lines.mkString
    tokens.filter(token => totalLines.contains(token))
      .foreach(token => {
        val score = scores.getOrElse(token, 0)
        scores = scores.updated(token, score + Score.scoreBody)
      })

    val totalComments = comments.mkString
    tokens.filter(token => totalComments.contains(token))
      .foreach(token => {
        val score = scores.getOrElse(token, 0)
        scores = scores.updated(token, score + Score.scoreComment)
      })

    val totalKeywords = keywords.mkString
    tokens.filter(token => totalKeywords.contains(token))
      .foreach(token => {
        val score = scores.getOrElse(token, 0)
        scores = scores.updated(token, score + Score.scoreTag)

      })


    val contain = scores.toList.forall(_._2 > 0)
    val totalScore = scores.toList.map(_._2).sum

    if (contain) {
      List(Hit(renderHtml(tokens), totalScore))
    } else {
      List(Hit("", 0))
    }
  }


  def searchComment(tokens: List[String]): List[Hit] = {
    val body = comments.mkString

    val contain = tokens.forall(token => {
      body.contains(token)
    })

    if (contain) {
      List(Hit(renderHtml(tokens), Score.scoreComment))
    } else {
      List(Hit("", 0))
    }
  }

  def searchBody(tokens: List[String]): List[Hit] = {
    val body = lines.mkString

    val contain = tokens.forall(token => {
      body.contains(token)
    })

    if (contain) {
      List(Hit(renderHtml(tokens), Score.scoreBody))
    } else {
      List(Hit("", 0))
    }
  }

  def searchKeywords(tokens: List[String]): List[Hit] = {
    var contain = false
    for (keyword <- keywords) {
      contain = contain || tokens.exists(token => {
        keyword.trim.contains(token.trim)
      })
    }

    if (contain) {
      List(Hit(renderHtml(tokens), Score.scoreTag))
    } else {
      List(Hit("", 0))
    }
  }

  override def search(tokens: List[String], context: Option[String]): List[Hit] = {
    context match {
      case Some("keyword") => searchKeywords(tokens)
      case Some("comment") => searchComment(tokens)
      case Some("body") => searchBody(tokens)
      case _ => searchContent(tokens)
    }
  }

  override def toHtml: String = {
    renderHtml(List())
  }

  def pieceType: String = {
    ""
  }

  protected def renderHtml(tokens: List[String]): String = {
    val html = new StringBuilder
    //title 是可以直接看到的，fileName是鼠标悬停的时候显示
    html.append(Node("a", title).className("piece-title").title(fileName.getOrElse("")))

    if (keywords.size > 0) {
      html.append(Node("div", keywords.map(keyword => {
        Node("code", keyword).className("piece-keyword").toString()
      }).mkString(" ")).className("piece-keyword-container"))
    }

    lines.foreach(line => {
      var text = line
      tokens.foreach(token => {
        text = renderHit(text, token)
      })
      html.append(Node("p", text).className("piece-content"))
    })

    comments.foreach(comment => {
      var text = comment
      tokens.foreach(token => {
        text = renderHit(text, token)
      })
      html.append(Node("p", text).className("piece-comment"))
    })

    html.toString
  }
}
