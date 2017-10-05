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
    NoteBook.notes.foreach(note => {
      val re = note.search(tokens, context)
      all.append(re)
    })
    all.toString()
  }
}
