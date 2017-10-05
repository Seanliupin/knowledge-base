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

  def searchContent(tokens: List[String]) = {
    val body = lines.mkString

    val contain = tokens.forall(token => {
      body.contains(token)
    })

    if (contain) {
      renderHtml(tokens)
    } else {
      ""
    }
  }


  def searchComment(tokens: List[String]) = {
    val body = comments.mkString

    val contain = tokens.forall(token => {
      body.contains(token)
    })

    if (contain) {
      renderHtml(tokens)
    } else {
      ""
    }
  }

  def searchKeywords(tokens: List[String]) = {
    var contain = false
    for (keyword <- keywords) {
      contain = contain || tokens.exists(token => {
        keyword.contains(token)
      })
    }

    if (contain) {
      renderHtml(tokens)
    } else {
      ""
    }
  }

  override def search(tokens: List[String], context: Option[String]): String = {
    context match {
      case Some("keyword") => searchKeywords(tokens)
      case Some("comment") => searchComment(tokens)
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
      }).mkString(" ")))
    }

    lines.foreach(line => {
      var tmpLine = line
      tokens.foreach(token => {
        tmpLine = renderHit(tmpLine, token)
      })
      html.append(Node("p", tmpLine).className("piece-content"))
    })
    html.toString
  }
}
