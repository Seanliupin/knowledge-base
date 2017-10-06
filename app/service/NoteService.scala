package service

import model.note.NoteBook

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 10:31 PM
  */
object NoteService {
  val pieces = NoteBook.notes.flatMap(note => note.pieces)

  def search(tokens: List[String], context: Option[String]): String = {
    val all = new StringBuilder

    pieces.flatMap(piece => piece.search(tokens, context))
      .filter(_.score > 0)
      .sortWith((x, y) => x.score > y.score)
      //.foreach(hit => all.append(hit.hit + " score = " + hit.score))
      .foreach(hit => all.append(hit.hit))
    all.toString()
  }
}
