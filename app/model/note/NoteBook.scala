package model.note

import java.io.File

/**
  * Author: Sean
  * Date: 4/10/2017
  * Time: 10:46 PM
  * 这个类表示这个笔记本
  */
object NoteBook {
  private val root = "/Users/seanliu/Note/"
//  private val root = "/Users/seanliu/Note/ztodo"  // for test

  def notes: List[Note] = {
    files.map(file => {
      Note(file.getAbsolutePath)
    })
  }

  private def files: List[File] = {
    @scala.annotation.tailrec
    def sc(acc: List[File], files: List[File]): List[File] = {
      files match {
        case Nil => acc
        case x :: xs => {
          val toIgnore = x.getName.startsWith(".")
          toIgnore match {
            case true => sc(acc, xs)
            case false => {
              x.isDirectory match {
                case false => sc(x :: acc, xs)
                case true => sc(acc, xs ::: x.listFiles.toList)
              }
            }
          }
        }
      }
    }

    sc(List(), List(new File(root)))
  }


}
