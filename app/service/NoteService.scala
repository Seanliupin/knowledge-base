package service

import model.note.NoteBook

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 10:31 PM
  */
object NoteService {

  def searchContent(tokens: List[String]): String = {
    val all = new StringBuilder
    NoteBook.notes.foreach(note => {
      val re = note.search(tokens, None)
      all.append(re)
    })
    all.toString()
  }


}
