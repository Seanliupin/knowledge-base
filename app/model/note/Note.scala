package model.note

import helper.StringUtil
import model.html.Category
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

        if (line.startsWith(Category.title)) {
          if (piece.isNotEmpty) {
            pieces = pieces ++ List(piece)
          }
          piece = Piece(line.replace(Category.title, ""), Option(fileName))
        } else if (piece.isNotEmpty) {
          if (line.startsWith(Category.keys)) {
            piece.addKeywords(line.replace(Category.keys, "").split(StringUtil.whiteSpaceSegmenter).toList.map(Line(_)))
          } else if (line.startsWith(Category.tags)) {
            piece.addKeywords(line.replace(Category.tags, "").split(StringUtil.whiteSpaceSegmenter).toList.map(Line(_)))
          } else if (line.startsWith(Category.comment)) {
            piece.addComment(line.replace(Category.comment, ""))
          } else {
            line match {
              case Extractor.WebExtractor(title, url, comment) =>
                piece.addWeb(Web(title, url, comment))
              case Extractor.BookExtractor(title, url, comment) =>
                piece.addBook(Book(title, url, comment))
              case _ => piece.addLine(line)
            }
          }
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
