package model

import model.html.{Html, Tag}
import resource.managed

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 10:48 PM
  * 这个类表示一篇笔记
  */
case class Note(fileName: String) extends Html {

  private def renderHit(text: String, token: String): String = {
    text.replaceAll(token, "<strong class=\"text-danger\">" + token + "</strong>")
  }

  def search(tokens: List[String]) = {
    val s = new StringBuilder
    getPiece.foreach(piece => {
      s.append(piece.search(tokens))
    })

    s.toString()
  }

  def getPiece: List[Piece] = {
    var pieces: List[Piece] = List()
    var piece = Piece("", None)

    for (source <- managed(scala.io.Source.fromFile(fileName))) {
      for (line <- source.getLines) {

        if (line.startsWith(Tag.title)) {
          if (piece.isNotEmpty) {
            pieces = pieces ++ List(piece)
          }
          piece = Piece(line.replace(Tag.title, ""), Option(fileName))
        } else if (piece.isNotEmpty) {
          if (line.startsWith(Tag.key)) {
            piece.addKeyword(line.replace(Tag.key, ""))
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
