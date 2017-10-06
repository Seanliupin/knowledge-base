package model.note

import model.html.Node

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 10:53 PM
  * 代表一则笔记
  */
case class Piece(title: Title, fileName: Option[String]) extends Searchable with Render {
  protected var lines: List[Line] = List()
  protected var keywords: List[KeyWord] = List()
  protected var comments: List[Comment] = List()
  protected var webs: List[Web] = List()
  protected var books: List[Book] = List()
  private var time: Option[Time] = None

  def addLine(line: Line) = {
    lines = lines ++ List(line)
  }

  def addKeyword(newKeyword: KeyWord) = {
    keywords = newKeyword +: keywords
  }

  def addComment(comment: Comment) = {
    comments = comments ++ List(comment)
  }

  def setTime(time: Time) = {
    this.time = Some(time)
  }

  def addWeb(web: Web): Unit = {
    webs = webs ++ List(web)
  }

  def addBook(book: Book): Unit = {
    books = books ++ List(book)
  }

  def isNotEmpty: Boolean = title.exist

  def searchContent(tokens: List[String]): List[HitScore] = {
    var scores: Map[String, Int] = tokens.map(token => (token, 0)).toMap

    tokens.filter(token => title.hit(token))
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

    html.append(Node("div", "" + title.toHtml(tokens, fileName.getOrElse("")) + time.getOrElse(Time("")).toHtml(tokens)).className("piece-title-box"))

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
