package service

import model.NoteBook

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 10:31 PM
  */
object NoteService {

  def search(tokens: List[String]): String = {
    val all = new StringBuilder
    NoteBook.notes.foreach(note => {
      val re = note.search(tokens)
      all.append(re)
    })
    all.toString()
  }
}
