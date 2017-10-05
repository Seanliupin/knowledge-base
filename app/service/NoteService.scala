package service

import model.note.NoteBook

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 10:31 PM
  */
object NoteService {

  def search(tokens: List[String], context: Option[String]): String = {
    val all = new StringBuilder

    NoteBook.notes.flatMap(note => {
      note.search(tokens, context)
    }).sortBy(_.score).foreach(hit => all.append(hit.hit + " score = " + hit.score))
    all.toString()
  }
}
