package model

import model.html.{Html, Node}

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 10:53 PM
  * 代表一则笔记
  */
case class Piece(title: String, fileName: Option[String]) extends Html {
  var lines: List[String] = List()
  var keywords: List[String] = List()

  private def renderHit(text: String, token: String): String = {
    text.replaceAll(token, "<strong class=\"text-danger\">" + token + "</strong>")
  }

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

  def addLine(line: String) = {
    lines = lines ++ List(line)
  }

  def addKeyword(keyword: String) = {
    keywords = keywords ++ List(keyword)
  }

  def isNotEmpty: Boolean = title.trim.length > 0

  override def toHtml: String = {
    renderHtml(List())
  }

  private def renderHtml(tokens: List[String]): String = {
    val html = new StringBuilder
    //title 是可以直接看到的，fileName是鼠标悬停的时候显示
    html.append(Node("a", title).className("piece-title").title(fileName.getOrElse("")))

    if (keywords.size > 0) {
      html.append(Node("div", keywords.mkString(", ")).className("piece-keyword"))
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
