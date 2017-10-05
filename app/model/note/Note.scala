package model.note

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
    * parse piece of infomation
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
            piece.addKeyword(line.replace(Category.keys, ""))
          } else if (line.startsWith(Category.tags)) {
            piece.addKeyword(line.replace(Category.tags, ""))
          } else if (line.startsWith(Category.comment)) {
            piece.addComment(line.replace(Category.comment, ""))
          } else {
            piece.addLine(line)
          }
        }
      }
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

  override def search(tokens: List[String], context: Option[String]): String = {
    val s = new StringBuilder
    getPiece.foreach(piece => {
      s.append(piece.search(tokens, context))
    })

    s.toString()
  }
}
