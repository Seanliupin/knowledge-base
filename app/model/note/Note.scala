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

  def searchContent(tokens: List[String]) = {
    val s = new StringBuilder
    getPiece.foreach(piece => {
      s.append(piece.searchContent(tokens))
    })

    s.toString()
  }

  def getPiece: List[Piece] = {
    var pieces: List[Piece] = List()
    var piece = Text("", None)

    for (source <- managed(scala.io.Source.fromFile(fileName))) {
      for (line <- source.getLines) {

        if (line.startsWith(Category.title)) {
          if (piece.isNotEmpty) {
            pieces = pieces ++ List(piece)
          }
          piece = Text(line.replace(Category.title, ""), Option(fileName))
        } else if (piece.isNotEmpty) {
          if (line.startsWith(Category.keys)) {
            piece.addKeyword(line.replace(Category.keys, ""))
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
}
