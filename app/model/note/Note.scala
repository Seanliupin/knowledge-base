package model.note

import helper.StringUtil
import resource.managed

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 10:48 PM
  * 这个类表示一篇笔记
  */
case class Note(fileName: String) extends Searchable {

  /**
    * parse piece of information
    **/
  def getPiece: List[Piece] = {
    var pieces: List[Piece] = List()
    var piece = Piece("", None)

    for (source <- managed(scala.io.Source.fromFile(fileName))) {
      for (line <- source.getLines) {
        line match {
          case Extractor.titleExtractor(title) => {
            if (piece.isNotEmpty) {
              pieces = pieces ++ List(piece)
            }
            piece = Piece(title.trim, Option(fileName))
          }
          case Extractor.timeExtractor(time) if piece.isNotEmpty =>
            piece.setTime(time)
          case Extractor.WebExtractor(title, url, comment) if piece.isNotEmpty =>
            piece.addWeb(Web(title, url, comment))
          case Extractor.BookExtractor(title, url, comment) if piece.isNotEmpty =>
            piece.addBook(Book(title, url, comment))
          case Extractor.commentExtractor(comment) if piece.isNotEmpty =>
            piece.addComment(comment)
          case Extractor.timeExtractor(time) if piece.isNotEmpty =>
            piece.setTime(time)
          case Extractor.tagsExtractor(tags) if piece.isNotEmpty =>
            piece.addKeywords(tags.split(StringUtil.whiteSpaceSegmenter).toList.map(Line(_)))
          case Extractor.keysExtractor(keys) if piece.isNotEmpty =>
            piece.addKeywords(keys.split(StringUtil.whiteSpaceSegmenter).toList.map(Line(_)))
          case _ if piece.isNotEmpty => piece.addLine(line)
          case _ =>
        }
      }
    }

    if (piece.isNotEmpty) {
      pieces = pieces ++ List(piece)
    }

    pieces
  }

  override def toHtml: String = {
    val html = new StringBuilder
    getPiece.foreach(piece => {
      html.append(piece.toHtml)
    })

    html.toString
  }

  override def search(tokens: List[String], context: Option[String]): List[Hit] = {
    getPiece.flatMap(piece => {
      piece.search(tokens, context)
    }).filter(_.score > 0)
  }
}
