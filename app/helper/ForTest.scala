package helper

import java.util.Observable

import model.note.NoteBook

/**
  * Author: Sean
  * Date: 8/10/2017
  * Time: 12:03 PM
  */
object ForTest {
  def main(args: Array[String]): Unit = {
    WatchDir.watch(NoteBook.root, true, (_: Observable, notice: Any) => {
      print(Thread.currentThread().getId + " => " + notice)
    })
  }
}
