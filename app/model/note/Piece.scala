package model.note

import model.html.{Node, Render}

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 10:53 PM
  * 代表一则笔记
  */
case class Piece(title: String, fileName: Option[String]) extends Searchable with Render {
  protected var lines: List[Line] = List()
  protected var keywords: List[Line] = List()
  protected var comments: List[Comment] = List()
  protected var webs: List[Web] = List()
  protected var books: List[Book] = List()
  private var time: Option[String] = None

  def addLine(line: Line) = {
    lines = lines ++ List(line)
  }

  def addKeywords(newKeywords: List[Line]) = {
    keywords = keywords ++ newKeywords
  }

  def addComment(comment: Comment) = {
    comments = comments ++ List(comment)
  }

  def setTime(time: String) = {
    this.time = Some(time)
  }

  def addWeb(web: Web): Unit = {
    webs = webs ++ List(web)
  }

  def addBook(book: Book): Unit = {
    books = books ++ List(book)
  }

  def isNotEmpty: Boolean = title.trim.length > 0

  def searchContent(tokens: List[String]): List[Hit] = {
    var scores: Map[String, Int] = tokens.map(token => (token, 0)).toMap

    tokens.filter(token => title.toLowerCase.contains(token))
      .foreach(token => {
        val score = scores.getOrElse(token, 0)
        scores = scores.updated(token, score + Score.scoreTitle)
      })

    val totalLines = lines.mkString.toLowerCase
    tokens.filter(token => totalLines.contains(token))
      .foreach(token => {
        val score = scores.getOrElse(token, 0)
        scores = scores.updated(token, score + Score.scoreBody)
      })

    val totalComments = comments.mkString.toLowerCase
    tokens.filter(token => totalComments.contains(token))
      .foreach(token => {
        val score = scores.getOrElse(token, 0)
        scores = scores.updated(token, score + Score.scoreComment)
      })

    val totalKeywords = keywords.mkString.toLowerCase
    tokens.filter(token => totalKeywords.contains(token))
      .foreach(token => {
        val score = scores.getOrElse(token, 0)
        scores = scores.updated(token, score + Score.scoreTag)
      })

    tokens.filter(token => webs.exists(_.hit(token)))
      .foreach(token => {
        val score = scores.getOrElse(token, 0)
        scores = scores.updated(token, score + Score.scoreWeb)
      })

    tokens.filter(token => books.exists(_.hit(token)))
      .foreach(token => {
        val score = scores.getOrElse(token, 0)
        scores = scores.updated(token, score + Score.scoreBook)
      })


    val contain = scores.toList.forall(_._2 > 0)
    val totalScore = scores.toList.map(_._2).sum

    if (contain) {
      List(Hit(renderHtml(tokens), totalScore))
    } else {
      List(Hit("", 0))
    }
  }

  private def search(tokens: List[String], items: List[Hitable], scoreValue: Int): List[Hit] = {
    var scores: Map[String, Int] = tokens.map(token => (token, 0)).toMap

    tokens.filter(token => items.exists(_.hit(token)))
      .foreach(token => {
        val score = scores.getOrElse(token, 0)
        scores = scores.updated(token, score + scoreValue)
      })

    val contain = scores.toList.forall(_._2 > 0)
    val totalScore = scores.toList.map(_._2).sum

    if (contain) {
      List(Hit(renderHtml(tokens), totalScore))
    } else {
      List(Hit("", 0))
    }

  }

  override def search(tokens: List[String], context: Option[String]): List[Hit] = {
    context match {
      case Some("keyword") => search(tokens, keywords, Score.scoreTag)
      case Some("comment") => search(tokens, comments, Score.scoreComment)
      case Some("web") => search(tokens, webs, Score.scoreWeb)
      case Some("book") => search(tokens, books, Score.scoreBook)
      case Some("body") => search(tokens, lines, Score.scoreBody)
      case Some("all") => searchContent(tokens)
      case _ => List()
    }
  }

  protected def renderHtml(tokens: List[String]): String = {
    val html = new StringBuilder
    //title 是可以直接看到的，fileName是鼠标悬停的时候显示
    val titleNode = Node("a", title).className("piece-title").title(fileName.getOrElse(""))
    val timeNode = Node("text", time.getOrElse("")).className("piece-time")
    html.append(Node("div", "" + titleNode + timeNode).className("piece-title-box"))

    if (keywords.size > 0) {
      html.append(Node("div", keywords.map(keyword => {
        Node("code", keyword.toString).className("piece-keyword").toString()
      }).mkString(" ")).className("piece-keyword-container"))
    }

    webs.foreach(web => {
      html.append(web.toHtml(tokens))
    })

    books.foreach(book => {
      html.append(book.toHtml(tokens))
    })

    lines.foreach(line => {
      html.append(line.toHtml(tokens))
    })

    comments.foreach(line => {
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
